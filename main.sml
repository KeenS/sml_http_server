structure Format = Ponyo_Format
structure Request  = Ponyo_Net_Http_Request
structure Response = Ponyo_Net_Http_Response

val MAX_CONN : int ref = ref ~1

structure Handle = struct
    open Socket
    type 'af t = {
        conn: ('af, active stream) sock,
        handler: Request.t -> Response.t,
        res: Response.t option,
        close: bool
    }

    fun new handler conn = {
        res = NONE,
        handler = handler,
        conn = conn,
        close = true
    }

    fun setResponse (t:'af t) res = {
        res = SOME(res),
        handler = #handler t,
        conn = #conn t,
        close = #close t
    }

    fun handleRequest (t:'af t) req = let
        val res = (#handler t) req
        val t = setResponse t res
    in
        t
    end

    fun write (t: 'af t) = (Option.map (fn res => Response.write(#conn t, res)) (#res t); ())

    fun close (t:'af t) = Socket.close (#conn t)
end


structure HandleManager = struct
    open Socket
    type 'af read_pool = (Socket.sock_desc * 'af Handle.t) list
    type 'af write_pool = (Socket.sock_desc * 'af Handle.t) list
    type 'af t = 'af read_pool * 'af write_pool
    val timeout = SOME(Time.fromSeconds(LargeInt.fromInt 0))
    fun new (): 'af t = ([], [])

    fun isEmpty ((r, w):'af t) = List.null r andalso List.null w

    fun add ((r, w):'af t) hndle = ((Socket.sockDesc (#conn hndle), hndle) :: r, w)

    fun part pool socks = List.partition (fn x => (List.exists (fn desc => sameDesc(desc, #1 x)) socks)) pool

    fun handleRead (r:'af read_pool) socks: ('af write_pool  * 'af read_pool) = let
        val (ready, incompletes) = part r socks
        val () = print ("in read: active: "  ^ (Int.toString (List.length ready)) ^ ", inactive: " ^ (Int.toString (List.length incompletes)) ^ "\n")

        fun f (desc, hndle) = let
            val () = print "before\n"
            val req = Request.read (#conn hndle)
            val () = print "after\n"
            val hndle = Handle.handleRequest hndle req
        in
            Format.println [Request.marshall req];
            (desc, hndle)
        end

        val completes =  List.map f ready
    in
        (completes, incompletes)
    end


    fun handleWrite (w:'af write_pool) socks : ('af read_pool * 'af write_pool) = let
        val (ready, incompletes) = part w socks
        val () = print ("in write: active: "  ^ (Int.toString (List.length ready)) ^ ", inactive: " ^ (Int.toString (List.length incompletes)) ^ "\n")

        fun f (desc, hndle) = let
            val () = Handle.write hndle
        in
            Handle.close hndle;
            false
        end

        val completes =  List.filter f ready
    in
        (completes, incompletes)
    end

    fun process ((r, w):'af t) {rds = rds, wrs = wrs, exs = exs} = let
        val () = print "called\n"
        val (rcmp, ricmp) = handleRead r rds
        val () = print "after read\n"
        val (wcmp, wicmp) = handleWrite (w) (wrs)
        val () = print "after write\n"
    in
        (ricmp @ wcmp, wicmp @ rcmp)
    end
    fun toSelect ((r, w): 'af t) (time: Time.time option) = {
        rds = List.map #1 r,
        wrs = List.map #1 w,
        exs = [],
        timeout = time
    }

    fun handleConns (t: 'af t): 'af t = process t (Socket.select (toSelect t timeout))

end

fun serve (sock, mng, handler) : unit =
  let
      val newHandle = Handle.new handler
      fun loop (sock, mng) = let
          val () = print "accepting\n"
          val mng = if HandleManager.isEmpty mng
                     then (case Socket.accept sock of (conn, _) => HandleManager.add mng (newHandle conn)) before print "blocking\n"
                     else (case Socket.acceptNB sock of
                              SOME((conn, _)) => HandleManager.add mng (newHandle conn)
                            | NONE => mng) before print "nb\n"

          val () = print "accepted\n"
      in
          loop (sock, HandleManager.handleConns mng);
          ()
      end
  in
      loop (sock, mng)
  end

fun listenAndServe (address: string, port: int) : unit =
  let
      val sock = INetSock.TCP.socket ();
  in
      Format.printf "Binding server...\n" [];
      Socket.bind (sock, INetSock.any port);
      Format.printf "Server bound. Listening on port %:%\n\n" [address, Int.toString port];
      Socket.listen (sock, !MAX_CONN);
      Socket.Ctl.setREUSEADDR (sock, true);
      serve (sock, HandleManager.new(), fn _ => Response.new "ok");
      Socket.close (sock);
      ()
  end



fun main () =
  listenAndServe ("127.0.0.1", 8000)
