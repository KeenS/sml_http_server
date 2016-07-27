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
    val request = Request.read (conn);
    val response = Response.new "ok"
in
    Format.println [Request.marshall request];
    response
end

fun add (t:'af t) (desc, sock): 'af t = (desc, sock) :: t
fun read (t:'af t) socks: {complete: 'af WritePool.t, incomplete: 'af t} = let
    val (ready, incompletes) = List.partition (fn x => (List.exists (fn desc => sameDesc(desc, #1 x)) socks)) t
    val completes =  List.map (fn (desc, conn) => (desc, conn, handler conn)) ready
in
    {complete = completes, incomplete = incompletes}
end
end

structure WritePool = struct

open WritePool

fun empty (): 'af t = []
fun isEmpty (t:'af t) = List.null t
fun add (t:'af t) (desc, sock, res): 'af t = (desc, sock, res) :: t

fun write (t:'af t) socks : 'af t = let
    fun f (desc, conn, res) =
      if List.exists (fn x => sameDesc(desc, x)) socks
      then (
          Response.write(conn, res)
        ; Socket.close conn
        ; false)
      else true
in
    List.filter f t
end

end

structure ConnPool = struct
type 'af t = 'af ReadPool.t * 'af WritePool.t
fun empty (): 'af t = (ReadPool.empty(), WritePool.empty())
fun isEmpty ((r, w):'af t) = ReadPool.isEmpty r andalso WritePool.isEmpty w
fun add ((r, w):'af t) conn = (ReadPool.add r (Socket.sockDesc conn, conn), w)
fun process ((r, w):'af t) (rds, wrs) = let
    val {complete=rcmp, incomplete=ricmp} = ReadPool.read r rds
    val wicmp = WritePool.write (w @ rcmp) (wrs)
in
    (ricmp, wicmp)
end
fun toSelect (t: 'af t) (time: Time.time option) = {
    rds = List.map #1 (#1 t),
    wrs = List.map #1 (#2 t),
    exs = [],
    timeout = time
}

end

fun serve (sock, pool) : unit =
  let
      val pool = if ConnPool.isEmpty pool
                 then case Socket.accept sock of (conn, _) => ConnPool.add pool conn
                 else case Socket.acceptNB sock of
                          SOME((conn, _)) => ConnPool.add pool conn
                        | NONE => pool
      val readys = Socket.select (ConnPool.toSelect pool (SOME(Time.fromSeconds(LargeInt.fromInt 0))))
      val pool = ConnPool.process pool (#rds readys, #wrs readys)
  in
      serve (sock, pool);
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
