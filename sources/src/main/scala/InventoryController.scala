import Main.info
import Utility.showAlert
import io.udash.wrappers.jquery.{EventName, JQueryEvent, jQ}
import org.scalajs.dom.raw.HTMLFormElement
import org.scalajs.dom.{Element, document}
import scalatags.JsDom.all._

import JQBootstrapped.jq2modalized

import scala.Ordering.Double.TotalOrdering

object InventoryController
{

    var inventorySortByColumn: Int = 0

    /** */
    def readyInventory(): Unit =
    {
        jQ("#add-item-button").on(EventName.click, openNewItemModal)
        jQ("#create-new-item").on(EventName.click, createupdateItem)
        jQ("#delete-item").on(EventName.click, removeItem)
        
        jQ("#inventory-search").on(EventName.keyUp, inventorySearch).value("")
    }

    /** */
    def updateInventory(): Unit =
    {
        val inventory = (inventorySortByColumn match
        {
            case 1 => info.inventory.sortBy(i => i.price * Mappings.currencies(i.priceUnit)).reverse
            case 2 => info.inventory.sortBy(_.weight).reverse
            case _ => info.inventory.sortBy(_.name)
        }).toList

        // Generate table
        jQ("#inventory-container").html(
            table(id := "inventory-table", cls := "table")(
                tr(
                    th(s"${if (inventorySortByColumn == 0) "⮚" else ""}Item"),
                    th(s"${if (inventorySortByColumn == 1) "⮚" else ""}Price"),
                    th(s"${if (inventorySortByColumn == 2) "⮚" else ""}Weight")
                ),
                for ((item, index) <- inventory.zipWithIndex)
                    yield tr(data.index := index, data.hash := item.##)(
                        td(s"${item.amount} ${item.name}"),
                        td(if (item.price != 0) s"${item.amount.toIntOption.getOrElse(1) * item.price} ${item.priceUnit}" else "-"),
                        td(if (item.weight != 0) item.amount.toIntOption.getOrElse(1) * item.weight else "-")
                    ),
            ).render
        ).find("tr:not(:first-child)").on(EventName.click, modifyItem)

        // Add event handler for sorting
        jQ("#inventory-table th:eq(0)").on(EventName.click, (_, _) =>
        {
            inventorySortByColumn = 0
            updateInventory()
        })
        jQ("#inventory-table th:eq(1)").on(EventName.click, (_, _) =>
        {
            inventorySortByColumn = 1
            updateInventory()
        })
        jQ("#inventory-table th:eq(2)").on(EventName.click, (_, _) =>
        {
            inventorySortByColumn = 2
            updateInventory()
        })

        // Show the search bar when there are at least 4 items
        if (info.inventory.length > 3)
            jQ("#inventory-search").show()
        else
            jQ("#inventory-search").hide()
    }

    /** Shows and hides items in the inventory depending on the entered search term. */
    private def inventorySearch(elem: Element, event: JQueryEvent): Unit =
    {
        val term = jQ(elem).value().asInstanceOf[String].toLowerCase

        jQ("#inventory-table tr").show()
        if (!term.isEmpty)
        {
            jQ("#inventory-table tr:not(:first-child)").each((elem, _) =>
            {
                val that = jQ(elem)
                if (!that.children().at(0).text().toLowerCase.contains(term))
                    that.hide()
            })
        }
    }

    /** */
    private def createupdateItem(elem: Element, event: JQueryEvent): Unit =
    {
        val formData = new FormData(document.getElementById("item-form").asInstanceOf[HTMLFormElement])
        val name = formData.get("name")

        if (name.isEmpty)
        {
            showAlert("#item-modal .alert-container", "Name is missing")
            return
        }

        val item = new Item(name, formData.get("amount"), formData.get("price").toIntOption.getOrElse(0), formData.get("priceUnit"), formData.get("weight").toDoubleOption.getOrElse(0), formData.get("notes"))

        formData.get("hash").toIntOption match
        {
            case Some(hash) => info.replaceItemByHash(hash, item)
            case None => info.addItem(item)
        }

        jQ("#item-modal").modal("hide")
        updateInventory()
    }

    /** */
    private def modifyItem(elem: Element, event: JQueryEvent): Unit =
    {
        val hash = jQ(elem).attr("data-hash").flatMap(_.toIntOption).get
        val item = info.item(hash)

        jQ("#item-modal [name=name]").value(item.name)
        jQ("#item-modal [name=price]").value(item.price)
        jQ("#item-modal [name=priceUnit]").value(item.priceUnit)
        jQ("#item-modal [name=weight]").value(item.weight)
        jQ("#item-modal [name=amount]").value(item.amount)
        jQ("#item-modal [name=notes]").value(item.notes)
        jQ("#item-modal [name=hash]").value(hash)
        jQ("#item-modal .alert").remove()
        jQ("#item-modal #create-new-item").text("Modify item")
        jQ("#delete-item").show()
        jQ("#item-modal").modal("show")
    }

    /** Removes the item currently opened in the modal. */
    private def removeItem(elem: Element, event: JQueryEvent): Unit =
    {
        val hash = jQ("#item-modal [name=hash]").value().asInstanceOf[String].toInt
        info.removeItem(hash)
        jQ("#item-modal").modal("hide")
        updateInventory()
    }

    /** */
    private def openNewItemModal(elem: Element, event: JQueryEvent): Unit =
    {
        jQ("#item-modal .modal-title").text("New item")
        jQ("#item-modal input").value("")
        jQ("#item-modal textarea").value("")
        jQ("#delete-item").hide()
        jQ("#item-modal .alert").remove()
        jQ("#item-modal").modal("show")
    }
}
