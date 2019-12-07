import scala.io.{Source, StdIn}

class Pelouse {
  //initialisation
  var LongueurPelouse=0
  var LargeurPelouse=0
  var taillePelouse=List[Int](0,0)
  var m:Int=0 /* m=0 => réponse juste: ne rester dans le bouble do while qu'une fois
                 m=1 => continue le boucle jusqu'à une saisie correcte */

  // Lire la longueur et la largeur dans la console
  def tailleSaisie{
    do {
      try {
        println("Longueur de la Pelouse? Merci de donner une saisie valide (exemple=>117)")
        LongueurPelouse = StdIn.readInt()
        m=if(LongueurPelouse.isValidInt && LongueurPelouse>0 )0 else 1
      } catch {
        case exception: NumberFormatException => m = 1
      }
    }while(m>0)
     m=0

    do {
      try{
        println ("Largeur de la Pelouse? Merci de donner une saisie valide (exemple=>45)")
        LargeurPelouse=StdIn.readInt()
        m=if(LargeurPelouse.isValidInt && LargeurPelouse>0 )0 else 1
      } catch{
        case exception: NumberFormatException =>m=1
      }
    }while(m>0)

    taillePelouse=List[Int](LargeurPelouse,LongueurPelouse)
    println("La taille de la pelouse est " + (LargeurPelouse,LongueurPelouse))
    println("*"*100)
  }


  // lire la longeur et la largeur dans le fichier de test
  def tailleLue{
    val commandes = Source.fromFile("Test_Projet_Scala.txt", "UTF-8").getLines().toArray
    var listeCommandes = commandes(0).replace(" ", "").toList
    LargeurPelouse=listeCommandes(0).asDigit
    LongueurPelouse=listeCommandes(1).asDigit
    taillePelouse=List[Int](LargeurPelouse,LongueurPelouse)
    println("La taille de la pelouse est " + (LargeurPelouse,LongueurPelouse))
    println("*"*100)
  }

}
