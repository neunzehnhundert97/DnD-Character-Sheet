import io.udash.wrappers.jquery.JQuery

import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope

@js.native
trait JQBootstrapped extends JQuery
{
    def modal(s: String): JQuery = js.native

    def alert(s: String): JQuery = js.native

    def popover(): JQuery = js.native
}

object JQBootstrapped
{
    implicit def jq2modalized(jq: JQuery): JQBootstrapped =
        jq.asInstanceOf[JQBootstrapped]
}

@js.native
@JSGlobalScope
object GlobalScope extends js.Object
{
    def encodeURIComponent(URI: String): String = js.native
}