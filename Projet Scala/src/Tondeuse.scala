import scala.collection.mutable
import scala.io.{Source, StdIn}
import scala.util.control._
import scala.collection.mutable.ListBuffer

class Tondeuse {
  var coordonneesTondeuse = mutable.Map("x" -> 2, "y" -> 2) // (x,y) de tondeuse
  var directionTondeuse = mutable.Map("Direction" -> "N")  // direction de tondeuse
  var j = 0 /*une variable qui permet de distinguer entre la ligne correspondant à la position
              et celle pour les commandes de déplacement dans le fichier de test, voir la fonction ChoixCritere*/
  var k:Int=1

  def ChoixCritere(critere: Int, numTondeuse: Int): Unit = {
    /* critere:
    1: position
    2: commandes
     */
    if (critere == 1 && numTondeuse == 1) {
      j = 1;
    } else if (critere == 1 && numTondeuse == 2) {
      j = 3;
    } else if (critere == 2 && numTondeuse == 1) {
      j = 2;
    } else {
      j = 4
    }
  }

  // fonction qui lit le fichier de test
  def LireFichierTest(numTondeuse: Int, xlimit: Int, ylimit: Int) {
    val source = Source.fromFile("Test_Projet_Scala.txt", "UTF-8").getLines().toArray

    ChoixCritere(1, numTondeuse: Int)
    val listePosition =source(j).toList
    validationDonnees(listePosition,xlimit,ylimit,numTondeuse)

    k=1
    ChoixCritere(2, numTondeuse: Int)
    val listeCommande = source(j).toList
    validationDonnees(listeCommande,xlimit,ylimit,numTondeuse)

    printf("La position FiNALE de la tondeuse %d est est (%d,%d,%s)\n",
      numTondeuse, coordonneesTondeuse("x"), coordonneesTondeuse("y"), directionTondeuse("Direction"))
    println("*"*100)

  }

  // fonction qui lit les saisies dans la console
  def lireSaisie(numTondeuse: Int, xlimit: Int, ylimit: Int): Unit ={
    do{
      val posiTondeuseSaisie = StdIn.readLine("""|Merci de saisir sa position en respectant la forme, (exemple=>3 4 N)
                                                 |si x ou y dépasse la taille de la pelouse: => x=Largeur_pelouse, y=Longueur_pelouse
                                                 |sinon le programme ne peut pas avancer
                                                 |saisir "Q" pour quitter le programme""".stripMargin).toList

      println("*"*100)
      validationDonnees(posiTondeuseSaisie,xlimit,ylimit,numTondeuse)
    }while(k>2)

    k=1
    do{
      val commandesTondeuseSaisies = StdIn.readLine("""|Merci de saisir les commandes pour la tondeuse en respectant la forme, (exemple=>AAADGAAAAA)
                                                       |sinon le programme ne peut pas avancer
                                                       |saisir "Q" pour quitter le programme """.stripMargin ).toList
      println("*"*100)
      //println(commandesTondeuseSaisies)
      validationDonnees(commandesTondeuseSaisies,xlimit,ylimit,numTondeuse)
    } while (k>2)

    printf("La position FiNALE de la tondeuse %d est est (%d,%d,%s)\n",
      numTondeuse, coordonneesTondeuse("x"), coordonneesTondeuse("y"), directionTondeuse("Direction"))
    println("*"*100)
  }

  /* fonction qui valide les inputs: position initiale?
  commandes de déplacement? commande pour quitter le programme? ou sasie non valide?*/
  def validationDonnees(donneesChar:List[Char], xlimit: Int, ylimit: Int,numTondeuse: Int): Unit = {
    val loop = new Breaks
    var xPosition = new StringBuilder()
    var yPosition = new StringBuilder()
    var donneesString=new ListBuffer[String]

    for (i<-donneesChar){ // convertir la liste[Char] en liste modifiable[String]
      donneesString.append(i.toString)
    }

    loop.breakable {
      if (donneesString.contains(" ")) { // si le input contient des espaces, il est soit la position initiale de la tondeuse soit une saisie non valide

        val espaceIndex: Int = donneesString.indexOf(" ", 0) //position de la première espace
        for (i <- 0 to espaceIndex-1) {
          try {
            donneesString(i).toInt
            if (donneesString(i).toInt>=0 ) {
              xPosition.append(donneesString(i)) // coordonnées "x"
            } else {
              loop.break()
            }
          } catch {
            case ex:NumberFormatException =>loop.break()
          }
        }

        try{
          val espaceIndex2: Int = donneesString.indexOf(" ", espaceIndex+1) // on ne sait pas s'il existe une deuxième espace, si non, c'est une saisie non valide
          for (i <- espaceIndex+1 to espaceIndex2-1) {
            donneesString(i).toInt
            if (donneesString(i).toInt>=0) {
              yPosition.append(donneesString(i)) // coordonnées "y"
            } else{
              loop.break()
            }
          }
        } catch{
          case ex:NumberFormatException => loop.break()
        }

        // direction initiale
        if (donneesString.last == "N" || donneesString.last == "W" || donneesString.last == "S" || donneesString.last == "E") { // direction
          directionTondeuse("Direction") = donneesString.last
        } else {
          loop.break()
        }

        k = 2 //si k=2, c-à-d la ligne saisie/lue concerne la position de tondeuse, on va ensuite printer les résultats finaux
        // x et y ne dépassent pas leurs limites
        coordonneesTondeuse("x") = if (Integer.valueOf(xPosition.toString()) < xlimit) Integer.valueOf(xPosition.toString()) else xlimit
        coordonneesTondeuse("y") = if (Integer.valueOf(yPosition.toString()) < ylimit) Integer.valueOf(yPosition.toString()) else ylimit
        printf("La position INITIALE de la Tondeuse %d est(%d,%d,%s)\n",
          numTondeuse, coordonneesTondeuse("x"), coordonneesTondeuse("y"), directionTondeuse("Direction"))
        println("*" * 100)
        loop.break()
      } else {
        loop.break()
      }
      loop.break()
    }


    var nombreCommandes:Int=0
    if (k!=2 && donneesString(0) == "Q" && donneesString.length == 1) { //sasir"Q" pour quitter
      System.exit(1)
    } else if(k!=2) { //sasir/lire une liste de commandes de deplacement correcte
      for (element <- donneesString) {
        if (element == "D" || element == "G" || element == "A") {
          nombreCommandes=nombreCommandes+1
        } else { //sasir/lire n'importe quoi, donc le programme n'avance pas => k=3, rester dans le boucle
          k = 3
        }
      }
    }


    if (nombreCommandes==donneesString.length){ //si la liste des commandes de déplacement est valide, execution
      for (element<-donneesString){
        deplacement(element.toString,xlimit,ylimit)
      }
      k=1
    }
  }


  def deplacement(commande: String, xlimit: Int, ylimit: Int): Unit = {
    commande match {
      case "D" => tournerADroite
      case "G" => tournerAGauche
      case "A" => avancer(xlimit, ylimit)
    }
    printf("Commande %s: position de la tondeuse (%d,%d,%s)\n", commande, coordonneesTondeuse("x"),coordonneesTondeuse("y"),directionTondeuse("Direction"))
  }

  def tournerAGauche: Unit = {
    directionTondeuse("Direction") match {
      case "N" => directionTondeuse("Direction") = "W"
      case "S" => directionTondeuse("Direction") = "E"
      case "W" => directionTondeuse("Direction") = "S"
      case "E" => directionTondeuse("Direction") = "N"
    }
  }

  def tournerADroite: Unit = {
    directionTondeuse("Direction") match {
      case "N" => directionTondeuse("Direction") = "E"
      case "S" => directionTondeuse("Direction") = "W"
      case "W" => directionTondeuse("Direction") = "N"
      case "E" => directionTondeuse("Direction") = "S"
    }
  }

  def avancer(xlimit: Int, ylimit: Int) {
    directionTondeuse("Direction") match {
      case "N" => if (coordonneesTondeuse("y") < ylimit) {coordonneesTondeuse("y") += 1
      } else {printf("(%d,%d,%s), La tondeuse ne peut plus avancer en dehors de la pelouse\n", coordonneesTondeuse("x"),coordonneesTondeuse("y"),directionTondeuse("Direction"))
      }
      case "S" => if (0 < coordonneesTondeuse("y")) {coordonneesTondeuse("y") -= 1
      } else {printf("(%d,%d,%s), La tondeuse ne peut plus avancer en dehors de la pelouse\n", coordonneesTondeuse("x"),coordonneesTondeuse("y"),directionTondeuse("Direction"))
      }
      case "W" => if (0 < coordonneesTondeuse("x")) {coordonneesTondeuse("x") -= 1
      } else {printf("(%d,%d,%s), La tondeuse ne peut plus avancer en dehors de la pelouse\n", coordonneesTondeuse("x"),coordonneesTondeuse("y"),directionTondeuse("Direction"))
      }
      case "E" => if (coordonneesTondeuse("x") < xlimit) {coordonneesTondeuse("x") += 1
      } else {printf("(%d,%d,%s), La tondeuse ne peut plus avancer en dehors de la pelouse\n", coordonneesTondeuse("x"),coordonneesTondeuse("y"),directionTondeuse("Direction"))
      }
    }
  }
}
