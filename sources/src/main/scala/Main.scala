import io.udash.wrappers.jquery._
import org.scalajs.dom.{Element, document, window}
import scalatags.JsDom.all._
import JQBootstrapped.jq2modalized
import org.scalajs.dom.raw.HTMLTextAreaElement
import GlobalScope.encodeURIComponent
import WeaponController._
import InventoryController._
import StatusController._
import AbilityController._
import AttributeController._
import NoteController._
import org.scalajs.dom

import scala.scalajs.js

object Main
{
    /** List of D&D dice in ascending order of sides. */
    val dice: List[String] = List("d4", "d6", "d8", "d10", "d12", "d20")

    var info: MainInformation = _

    /** Entry point */
    def main(args: Array[String]): Unit =
    {
        // Register ready handler
        // This is done in this manner as onReady without parenthesis would be called instead of being passed
        jQ(() => onReady())
    }

    /** Ready function to be executed when the document is loaded. Loads config, binds handlers and displays character information. */
    def onReady(): Unit =
    {
        initializeInformation()
        updateAll()

        // Show a warning when trying to reload the page
        window.addEventListener("beforeunload", (e: js.Dynamic) => e.preventDefault())

        // Define click handler for buttons
        jQ("#button-import").on(EventName.click, openImportModal)
        jQ("#import-string-button").on(EventName.click, importHandler)
        jQ("#button-export").on(EventName.click, openExportModal)
        jQ("#button-save").on(EventName.click, saveHandler)
        jQ("#button-options").on(EventName.click, openOptionsModal)

        // Set click handler for general information
        jQ("#general-stats").on(EventName.click, openGeneralModal)

        // Change handler for elements in general modal
        jQ("#general-modal input[name=name]").on(EventName.change, nameHandler)
        jQ("#general-modal select[name=level]").on(EventName.change, levelHandler)
        jQ("#general-modal input[name=experience]").on(EventName.change, experienceHandler)
        jQ("#general-modal input[name=class]").on(EventName.change, classHandler)
        jQ("#general-modal input[name=race]").on(EventName.change, raceHandler)
        jQ("#general-modal select[name=casterType]").on(EventName.change, casterHandler)
        jQ("#general-modal #is-jack-of-all-trades").on(EventName.change, jackHandler)

        // Call ready function of controllers
        readyAttributes()
        readyAbilities()
        readyInventory()
        readyStatuses()
        readyWeapons()
        readyNotes()
    }

    /** Loads information embedded in the documents or the default. */
    private def initializeInformation(): Unit =
    {
        // Load embedded JSON string from HTML
        val embeddedInfo = jQ("#information-container").text().replace("\n", "")
        var loadSuccessful = false

        // Try to process it if existing
        try
        {
            if (!embeddedInfo.isEmpty)
            {
                info = MainInformation.fromJSON(embeddedInfo)
                loadSuccessful = true
                println("Loaded embedded information")
            }
        }
        catch
        {
            case e: Throwable =>
                println("There was an error while loading the embedded information: " + e.getCause)
        }

        // If there was no information string or it was invalid, the default information object is created
        if (!loadSuccessful)
        {
            println("Loading default information")
            info = new MainInformation()
        }
    }

    /** Performs all updates. */
    def updateAll(): Unit =
    {
        updateTitle()
        updateAttributes()
        updateAbilities()
        updateWeaponList()
        updateInventory()
        updateHealth()
        updateHitDie()
        updateCustomStatuses()
        updateNotes()
    }

    /** Updates the document's title and navbar. */
    def updateTitle(): Unit =
    {
        // Load header information
        document.title = "D&D Sheet - " + info.name
        jQ("#general-stats").text(s"${info.name}, ${info.level} Level ${info.cls} (${info.race})")
    }

    /** Handles a changed class. */
    private def classHandler(elem: Element, event: JQueryEvent): Unit =
        info.cls = jQ(elem).value().asInstanceOf[String]

    /** Handles changed experience. */
    private def experienceHandler(elem: Element, event: JQueryEvent): Unit =
    {
        val xp = jQ(elem).value().asInstanceOf[String].toIntOption.getOrElse(0)

        // Find matching level
        val level: Int = Mappings.levels.toList.sorted.reverse.find(map => map._2._1 < xp).map(_._1).getOrElse(1)

        // Set xp and level
        info.experience = xp
        info.level = level

        // Update shown info
        jQ("#general-modal select[name=level]").value(level)
    }

    /** Handles a changed level. */
    private def levelHandler(elem: Element, event: JQueryEvent): Unit =
    {
        val level = jQ(elem).value().asInstanceOf[String].toInt

        // Update info object
        info.level = level
        info.experience = Mappings.levels(level)._1

        // Update shown info
        jQ("#general-modal input[name=experience]").value(Mappings.levels(level)._1)
    }

    /** Handles a changed name. */
    private def nameHandler(elem: Element, event: JQueryEvent): Unit =
        info.name = jQ(elem).value().asInstanceOf[String]

    /** Handles a changed race. */
    private def raceHandler(elem: Element, event: JQueryEvent): Unit =
        info.race = jQ(elem).value().asInstanceOf[String]

    /** Handles a caster type. */
    private def casterHandler(elem: Element, event: JQueryEvent): Unit =
        info.casterType = jQ(elem).value().asInstanceOf[String]

    /** Handles a changed jack of all trades property. */
    private def jackHandler(elem: Element, event: JQueryEvent): Unit =
        info.jackOfAllTrades = jQ(elem).prop("checked").asInstanceOf[Boolean]

    /** Open the import modal. */
    private def openImportModal(elem: Element, event: JQueryEvent): Unit =
    {
        jQ("#import-string").value("")
        jQ("#import-modal").modal("show")
    }

    /** Tries to import and parse the given JSON string. */
    private def importHandler(elem: Element, event: JQueryEvent): Unit =
        try
        {
            val input = jQ("#import-string").value().asInstanceOf[String]
            info = MainInformation.fromJSON(input)
            jQ("#import-modal").modal("hide")

            // Apply read information
            updateAll()
        } catch
        {
            case e: Throwable =>
                window.alert("Malformed input!")
        }


    /** Shows the export modal, fills in the string and selects it. */
    private def openExportModal(elem: Element, event: JQueryEvent): Unit =
    {
        // Set export string
        jQ("#export-string").text(info.toJSON)
        // Display modal
        jQ("#export-modal").modal("show")
        // Select all text after short delay
        window.setTimeout(() => jQ("#export-string").get(0) match
        {
            case Some(i: HTMLTextAreaElement) =>
                i.focus()
                i.select()
            case _ =>
        }, 200)
    }

    /** Opens the modal for general information. */
    private def openGeneralModal(elem: Element, event: JQueryEvent): Unit =
    {
        jQ("#general-modal input[name=name]").value(info.name)
        jQ("#general-modal select[name=level]").value(info.level)
        jQ("#general-modal input[name=experience]").value(info.experience)
        jQ("#general-modal input[name=class]").value(info.cls)
        jQ("#general-modal input[name=race]").value(info.race)
        jQ("#general-modal input[name=race]").value(info.race)
        jQ("#general-modal select[name=casterType]").value(info.casterType)
        jQ("#general-modal #is-jack-of-all-trades").prop("checked", info.jackOfAllTrades)
        jQ("#general-modal").modal("show")
    }

    /** Shows a save dialog. */
    private def saveHandler(elem: Element, event: JQueryEvent): Unit =
    {
        // Write information into file
        jQ("#information-container").text(info.toJSON)

        // Generate file content out of the document
        val content = "data:plain/attachment," + encodeURIComponent("<!DOCTYPE HTML>" + document.documentElement.outerHTML)

        // Create a element with download
        val element = a(href := content, attr("download") := s"${info.name} - Sheet.html", style := "display: none").render

        // Insert it, click it, and remove it
        document.body.appendChild(element)
        element.click()
        document.body.removeChild(element)
    }

    /** Shows the options modal. */
    private def openOptionsModal(elem: Element, event: JQueryEvent): Unit =
    {
        // Open the modal
        jQ("#options-modal").modal("show")
    }
}