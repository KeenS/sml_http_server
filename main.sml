structure Format = Ponyo_Format
structure Request  = Ponyo_Net_Http_Request
structure Response = Ponyo_Net_Http_Response

val MAX_CONN : int ref = ref ~1

structure WritePool = struct
open Socket
type 'af t = (sock_desc * ('af, active stream) sock * Response.t) list
end

structure ReadPool = struct
open Socket
type 'af t = (sock_desc * ('af, active stream) sock) list
fun empty (): 'af t = []
fun isEmpty (t:'af t) = List.null t
(* TODO: get from other place *)
fun handler conn =  let
    val request = Request.read conn
    val response = Response.new "ok"
in
    Format.println [Request.marshall request];
    response
end

fun add (t:'af t) (desc, sock): 'af t = (desc, sock) :: t

fun read (t:'af t) socks: ('af WritePool.t * 'af t) = let
    val (ready, incompletes) = List.partition (fn x => (List.exists (fn desc => sameDesc(desc, #1 x)) socks)) t
    val completes =  List.map (fn (desc, conn) => (desc, conn, handler conn)) ready
in
    (completes, incompletes)
end
end

structure WritePool = struct

open WritePool

fun empty (): 'af t = []
fun isEmpty (t:'af t) = List.null t
fun add (t:'af t) (desc, sock, res): 'af t = (desc, sock, res) :: t

fun write (t:'af t) socks : ('af t * 'af ReadPool.t) = let
    val (ready, incompletes) = List.partition (fn x => (List.exists (fn desc => sameDesc(desc, #1 x)) socks)) t

    fun f (desc, conn, res) = let
        val () = Response.write(conn, res)
    in
        Socket.close conn;
        false
    end

    val toRead =  List.filter f ready
in
    (incompletes, List.map (fn (desc, conn, res) => (desc, conn)) toRead)
end

end

structure ConnPool = struct
type 'af t = 'af ReadPool.t * 'af WritePool.t
val timeout = SOME(Time.fromSeconds(LargeInt.fromInt 0))
fun empty (): 'af t = (ReadPool.empty(), WritePool.empty())
fun isEmpty ((r, w):'af t) = ReadPool.isEmpty r andalso WritePool.isEmpty w
fun add ((r, w):'af t) conn = (ReadPool.add r (Socket.sockDesc conn, conn), w)
fun process ((r, w):'af t) {rds = rds, wrs = wrs, exs = exs} = let
    val (rcmp, ricmp) = ReadPool.read r rds
    val (wicmp, rready) = WritePool.write (w) (wrs)
in
    (ricmp @ rready, wicmp @ rcmp)
end
fun toSelect (t: 'af t) (time: Time.time option) = {
    rds = List.map #1 (#1 t),
    wrs = List.map #1 (#2 t),
    exs = [],
    timeout = time
}

fun handleConns (t: 'af t): 'af t = process t (Socket.select (toSelect t timeout))

end

fun serve (sock, pool) : unit =
  let
      val pool = if ConnPool.isEmpty pool
                 then case Socket.accept sock of (conn, _) => ConnPool.add pool conn
                 else case Socket.acceptNB sock of
                          SOME((conn, _)) => ConnPool.add pool conn
                        | NONE => pool

  in
      serve (sock, ConnPool.handleConns pool);
      ()
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
      serve (sock, ConnPool.empty());
      Socket.close (sock);
      ()
  end



fun main () =
  listenAndServe ("127.0.0.1", 8080)
