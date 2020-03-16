import Main.{updateAbilities, updateAttributes, updateWeaponList}

import scala.collection.mutable.{ListBuffer, Map => MutableMap, Set => MutableSet, Seq => MutableSeq}
import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.{Dictionary, JSON, |}

sealed trait Information

class MainInformation(_general: General = new General(),
                      _stats: MutableMap[String, Int] = MutableMap(
                          "strength" -> 10,
                          "dexterity" -> 10,
                          "constitution" -> 10,
                          "intelligence" -> 10,
                          "wisdom" -> 10,
                          "charisma" -> 10
                      ),
                      proficiency: MutableSet[String] = MutableSet(),
                      expertise: MutableSet[String] = MutableSet(),
                      _weapons: ListBuffer[Weapon] = ListBuffer()) extends Information
{

    def isProficient(ability: String): Boolean =
        proficiency.contains(ability.toLowerCase)

    def addProficiency(ability: String): Unit =
    {
        proficiency += ability.toLowerCase
        updateAttributes()
        updateAbilities()
    }

    def removeProficiency(ability: String): Unit =
    {
        proficiency -= ability.toLowerCase
        updateAttributes()
        updateAbilities()
    }

    def isExpert(ability: String): Boolean =
        expertise.contains(ability.toLowerCase)

    def addExpertise(ability: String): Unit =
        expertise += ability.toLowerCase

    def removeExpertise(ability: String): Unit =
        expertise -= ability.toLowerCase

    def proficiencyBonus: Int =
        Mappings.levels(_general.level)._2

    def name: String =
        _general.name

    def name_=(s: String): Unit =
        _general.name = s

    def race: String =
        _general.race

    def race_=(s: String): Unit =
        _general.race = s

    def `class`: String =
        _general.`class`

    def class_=(s: String): Unit =
        _general.`class` = s

    def level: Int =
        _general.level

    def level_=(i: Int): Unit =
    {
        _general.level = i
        updateAbilities()
        updateAttributes()
        updateWeaponList()
    }

    def experience: Int =
        _general.experience

    def experience_=(i: Int): Unit =
        _general.experience = i


    def score(attribute: String | Int): Int = attribute.asInstanceOf[Any] match
    {
        case s: String => _stats(s.toLowerCase.substring(0, 3))
        case i: Int => _stats.toList(i)._2
    }

    def score(attribute: String, value: Int): Unit =
    {
        attribute.asInstanceOf[Any] match
        {
            case s: String => _stats(s.toLowerCase.substring(0, 3)) = value
            case i: Int => _stats(_stats.toList(i)._1) = value
        }
        updateAbilities()
        updateWeaponList()
    }

    def weapon(index: Int): Weapon =
        _weapons(index)

    def weapons: List[Weapon] =
        _weapons.toList

    def appWeapon(weapon: Weapon): Unit =
        _weapons += weapon

    def replaceWeapon(index: Int, weapon: Weapon): Unit =
        _weapons(index) = weapon

    /** Returns the saved information as JSON string to be saved or exported. */
    def toJSON: String =
        JSON.stringify(MutableMap(
            "general" -> _general.toJSON,
            "stats" -> _stats.toJSDictionary,
            "proficiency" -> proficiency.toJSArray,
            "expertise" -> expertise.toJSArray,
            "weapons" -> _weapons.toJSArray
        ).toJSDictionary)
}

object MainInformation
{
    def fromJSON(json: String): MainInformation =
    {
        val data = JSON.parse(json).asInstanceOf[Dictionary[Any]]
        new MainInformation(
            General.fromJSON(data("general").asInstanceOf[Dictionary[Any]]),
            data("stats").asInstanceOf[Dictionary[Int]],
            data("proficiency").asInstanceOf[js.Array[String]].to(MutableSet),
            data("expertise").asInstanceOf[js.Array[String]].to(MutableSet),
            data("weapons").asInstanceOf[js.Array[Weapon]].to(ListBuffer)
        )
    }
}

class General(var name: String = "Name",
              var `class`: String = "Fighter",
              var race: String = "Cyber Elf",
              var level: Int = 1,
              var experience: Int = 0) extends Information
{
    /** Returns the saved information as JSON string to be saved or exported. */
    def toJSON: js.Dictionary[Any] =
        MutableMap(
            "name" -> name,
            "level" -> level,
            "class" -> `class`,
            "race" -> race,
            "experience" -> experience
        ).toJSDictionary
}

object General
{
    def fromJSON(json: Dictionary[Any]): General =
        new General(
            json("name").asInstanceOf[String],
            json("class").asInstanceOf[String],
            json("race").asInstanceOf[String],
            json("level").asInstanceOf[Int],
            json("experience").asInstanceOf[Int]
        )
}

class Weapon(var name: String,
             var die: String,
             var dieCount: Int,
             var melee: Boolean,
             var damageType: String,
             var notes: String,
             var hitBonus: Int,
             var damageBonus: Int,
             var proficiency: Boolean,
             var finesse: Boolean,
             var heavy: Boolean,
             var reach: Boolean,
             var loading: Boolean,
             var light: Boolean,
             var versatile: Boolean,
             var twoHanded: Boolean,
             var thrown: Boolean,
             var shortRange: Int,
             var longRange: Int) extends js.Object
