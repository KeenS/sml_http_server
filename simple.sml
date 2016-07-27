structure Server = Ponyo_Net_Http_Server
structure Response = Ponyo_Net_Http_Response

fun route req = Response.new "ok"
fun main () =
  Server.listenAndServe("127.0.0.1", 8090, route)
