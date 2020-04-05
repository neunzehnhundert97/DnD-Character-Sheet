import Main.info
import io.udash.wrappers.jquery.{EventName, JQueryEvent, jQ}
import org.scalajs.dom.{Element, document}
import scalatags.JsDom.all._
import JQBootstrapped.jq2modalized
import org.scalajs.dom.raw.HTMLFormElement
import Utility.showAlert

object NoteController
{
    /** Registers event handlers regarding notes. */
    def readyNotes(): Unit =
    {
        jQ("#add-note-button").on(EventName.click, openNoteModal)
        jQ("#create-new-note").on(EventName.click, createModifyNote)
        jQ("#delete-note").on(EventName.click, removeNote)
    }

    /** Updates the list of notes. */
    def updateNotes(): Unit =
    {
        jQ("#note-container").html(table(cls := "table", id := "note-table")(
            tr(th("Notes")),
            for (note <- info.notes.toList)
                yield tr(
                    td(note.title)
                )
        ).render)
            .find("tr:not(:first-child)").on(EventName.click, modifyNote)
    }

    /** Creates a new note or modifies an existing one. */
    private def createModifyNote(elem: Element, event: JQueryEvent): Unit =
    {
        val formData = new FormData(document.getElementById("note-form").asInstanceOf[HTMLFormElement])
        val title = formData.get("title")
        val oldTitle = formData.get("oldTitle")
        val modified = oldTitle != ""

        if (!modified && info.notes.exists(_.title == title))
        {
            showAlert("#note-modal .alert-container", "A note with the same title already exists.")
            return
        }

        if (modified)
        {
            info.notes.remove(info.notes.indexWhere(_.title == oldTitle))
            info.notes.append(new Note(title, formData.get("text")))
        }
        else
            info.notes.append(new Note(title, formData.get("text")))


        jQ("#note-modal").modal("hide")
        updateNotes()
    }

    /** Opens the note modal and inserts the selected note's values. */
    private def modifyNote(elem: Element, event: JQueryEvent): Unit =
    {
        val note = info.notes.find(_.title == jQ(elem).find("td").text()).get

        jQ("#note-modal [name=title]").value(note.title)
        jQ("#note-modal [name=text]").value(note.text)
        jQ("#note-modal [name=oldTitle]").value(note.title)
        jQ("#note-modal .modal-title").text("Modify note")
        jQ("#create-new-note").text("Modify note")
        jQ("#note-modal #delete-note").show()
        jQ("#note-modal").modal("show")
    }

    /** Removes the selected note. */
    private def removeNote(elem: Element, event: JQueryEvent): Unit =
    {
        val formData = new FormData(document.getElementById("note-form").asInstanceOf[HTMLFormElement])
        val title = formData.get("oldTitle")

        info.notes.remove(info.notes.indexWhere(_.title == title))
        jQ("#note-modal").modal("hide")
        updateNotes()
    }

    /** Opens the note modal and clears all inputs. */
    private def openNoteModal(elem: Element, event: JQueryEvent): Unit =
    {
        jQ("#note-modal [name=title]").value("")
        jQ("#note-modal [name=text]").value("")
        jQ("#note-modal [name=oldTitle]").value("")
        jQ("#note-modal .modal-title").text("Add note")
        jQ("#create-new-note").text("Add note")
        jQ("#note-modal #delete-note").hide()
        jQ("#item-modal .alert").remove()
        jQ("#note-modal").modal("show")
    }
}
