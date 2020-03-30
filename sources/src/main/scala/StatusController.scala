import Main.info
import Utility.showAlert
import io.udash.wrappers.jquery.{EventName, JQueryEvent, Selector, jQ}
import org.scalajs.dom.{Element, document}
import org.scalajs.dom.raw.{HTMLFormElement, WheelEvent}
import JQBootstrapped.jq2modalized
import scalatags.JsDom.all._

import scala.collection.mutable.{Map => MutableMap}

object StatusController
{

    var statusDiffs: MutableMap[String, Int] = MutableMap(
        "maxHP" -> 0,
        "currentHP" -> 0,
        "tempHP" -> 0,
        "usedHitDie" -> 0
    )

    def readyStatuses(): Unit =
    {
        jQ("#add-status-button").on(EventName.click, openNewStatusModal)
        jQ("#create-new-status").on(EventName.click, createUpdateCustomStatus)
        jQ("#delete-status").on(EventName.click, removeCustomStatus)

        // Handlers for health displays
        document.getElementById("health-max").addEventListener("wheel",
            wheelHandlerWithTempValue("maxHP", downCondition = _ > -info.maxHP))
        jQ("#health-max").on(EventName.click, clickHandlerWithTempValue("maxHP", updateHealth))

        document.getElementById("health-current").addEventListener("wheel",
            wheelHandlerWithTempValue("currentHP", upCondition = _ < info.maxHP - info.currentHP, downCondition = _ > -info.currentHP))
        jQ("#health-current").on(EventName.click, clickHandlerWithTempValue("currentHP", updateHealth))

        document.getElementById("health-temp").addEventListener("wheel",
            wheelHandlerWithTempValue("tempHP", downCondition = _ > -info.tempHP))
        jQ("#health-temp").on(EventName.click, clickHandlerWithTempValue("tempHP", updateHealth))

        document.getElementById("hit-die-used").addEventListener("wheel",
            wheelHandlerWithTempValue("usedHitDie", upCondition = _ < info.maxHitDie - info.usedHitDie, downCondition = _ > -info.maxHitDie))
        jQ("#hit-die-used").on(EventName.click, clickHandlerWithTempValue("usedHitDie", updateHitDie))
    }

    def updateHealth(): Unit =
    {
        jQ("#health-max span:eq(0)").text(info.maxHP)
        jQ("#health-max span:eq(1)").text("")
        jQ("#health-current span:eq(0)").text(info.currentHP)
        jQ("#health-current span:eq(1)").text("")
        jQ("#health-temp span:eq(0)").text(info.tempHP)
        jQ("#health-temp span:eq(1)").text("")
    }

    def updateHitDie(): Unit =
    {
        jQ("#hit-die-used span:eq(0)").text(info.usedHitDie)
        jQ("#hit-die-used span:eq(1)").text("")
        jQ("#hit-die-total span:eq(0)").text(info.maxHitDie)
        jQ("#hit-die-total span:eq(1)").text("")
    }

    def updateCustomStatuses(): Unit =
    {
        jQ("#custom-status-container").html(
            table(cls := "table", id := "custom-status-table")(
                tr(th("Name"), th("Current"), th("Max")),
                (for ((status, values) <- info.customStatus)
                    yield tr(td(status), td(span(values.current), span()), td(span(values.max), span()))).toList
            ).render
        ).find("tr:not(:first-child)").each((elem, _) =>
        {
            val jElem = jQ(elem)

            // Prepare keys
            val key = jElem.children().at(0).text()
            val keyMax = "max" + key
            val keyCurrent = "current" + key

            // Enter key entries in diffs
            statusDiffs(keyMax) = 0
            statusDiffs(keyCurrent) = 0

            // Register wheel handlers
            jElem.children().get(1).get.addEventListener("wheel",
                wheelHandlerWithTempValue(keyCurrent, downCondition = _ > -info.customStatus(key).current,
                    upCondition = _ < info.customStatus(key).max - info.customStatus(key).current))
            jElem.children().get(2).get.addEventListener("wheel",
                wheelHandlerWithTempValue(keyMax, downCondition = _ > -info.customStatus(key).max))

            // Register click handlers
            jElem.children().at(1).on(EventName.click, (_, _) => if (statusDiffs(keyCurrent) != 0)
            {
                info.customStatus(key).current += statusDiffs(keyCurrent)
                statusDiffs(keyCurrent) = 0
                updateCustomStatuses()
            })
            jElem.children().at(2).on(EventName.click, (_, _) => if (statusDiffs(keyMax) != 0)
            {
                info.customStatus(key).max += statusDiffs(keyMax)
                statusDiffs(keyMax) = 0
                updateCustomStatuses()
            })
        }).children().at(0).on(EventName.click, modifyCustomStatus)
    }

    /** */
    private def createUpdateCustomStatus(elem: Element, event: JQueryEvent): Unit =
    {
        val formData = new FormData(document.getElementById("custom-status-form").asInstanceOf[HTMLFormElement])
        val name = formData.get("name")

        if (name.isEmpty)
        {
            showAlert("#status-modal .alert-container", "Name is missing")
            return
        }

        val oldName = formData.get("oldName")

        if (oldName.isEmpty)
            info.customStatus(name) = new CustomStat(0, 0)
        else
        {
            info.customStatus(name) = info.customStatus(oldName)
            info.customStatus.remove(oldName)
        }

        jQ("#status-modal").modal("hide")
        updateCustomStatuses()
    }

    /** */
    private def modifyCustomStatus(elem: Element, event: JQueryEvent): Unit =
    {
        jQ("#status-modal .modal-title").text("Modify status")
        jQ("#create-new-status").text("Modify status")
        val name = jQ(elem).text()
        jQ("#status-modal input[name=name]").value(name)
        jQ("#status-modal input[name=oldName]").value(name)
        jQ("#delete-status").show()
        jQ("#status-modal").modal("show")
    }

    /** */
    private def removeCustomStatus(elem: Element, event: JQueryEvent): Unit =
    {
        val oldName = jQ("#custom-status-form [name=oldName]").value().asInstanceOf[String]
        info.customStatus.remove(oldName)
        jQ("#status-modal").modal("hide")
        updateCustomStatuses()
    }

    /** */
    private def wheelHandlerWithTempValue(key: String, selector: Selector = "span:eq(1)",
                                          upCondition: Int => Boolean = _ => true, downCondition: Int => Boolean = _ => true)
                                         (event: WheelEvent): Unit =
    {
        // Prevent scrolling the whole page
        event.preventDefault()

        // Detect scroll up
        if (event.deltaY < 0 && upCondition(statusDiffs(key)))
            statusDiffs(key) += 1
        // Detect scroll down
        else if (event.deltaY > 0 && downCondition(statusDiffs(key)))
            statusDiffs(key) -= 1

        if (statusDiffs(key) != 0)
            jQ(event.currentTarget).find(selector).text(f"${statusDiffs(key)}%+d")
                .attr("style", s"font-weight: bold; color: ${if (statusDiffs(key) > 0) "green" else "red"}")
        else
            jQ(event.currentTarget).find(selector).text("")
    }

    /** */
    private def clickHandlerWithTempValue(key: String, update: () => Unit)(elem: Element, event: JQueryEvent): Unit =
        if (statusDiffs(key) != 0)
        {
            info.status(key) += statusDiffs(key)
            statusDiffs(key) = 0
            update()
        }

    /** */
    private def openNewStatusModal(elem: Element, event: JQueryEvent): Unit =
    {
        jQ("#status-modal .modal-title").text("New status")
        jQ("#status-modal input").value("")
        jQ("#create-new-status").text("Add status")
        jQ("#delete-status").hide()
        jQ("#status-modal").modal("show")
    }
}
