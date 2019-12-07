import scala.io.StdIn

object Execution extends App {

  println ("""Vous souhaitez:
             |1. Saisir vos propres données
             |2. Utiliser le fichier de test""".stripMargin)

  var n:Int=0 /* n=0 => réponse juste: ne rester dans le bouble do while qu'une fois
                 n=1 => continue le boucle jusqu'à une saisie correcte*/
  var LaPelouse =new Pelouse
  var TondeuseUn= new Tondeuse
  var TondeuseDeux=new Tondeuse

  do {
    try {
      val reponse: Int = StdIn.readInt()
      reponse match {
        case 1 => LaPelouse.tailleSaisie
          println("Pour Tondeuse N°1: ")
          TondeuseUn.lireSaisie(1, LaPelouse.LargeurPelouse, LaPelouse.LongueurPelouse)
          println("Pour Tondeuse N°2: ")
          TondeuseDeux.lireSaisie(2, LaPelouse.LargeurPelouse, LaPelouse.LongueurPelouse)
          System.exit(0)
        case 2 => LaPelouse.tailleLue
          TondeuseUn.LireFichierTest(1, LaPelouse.LargeurPelouse, LaPelouse.LongueurPelouse)
          TondeuseDeux.LireFichierTest(2, LaPelouse.LargeurPelouse, LaPelouse.LongueurPelouse)
          System.exit(0)
        case _ => println("""Merci de choisir "1" ou "2" """)
          n=1
      }
    } catch {
      case exception: NumberFormatException => println("""Merci de saisir votre réponse correctement, il faut la réponse soit le chiffre "1" ou "2" """)
        n=1
    }

  }while(n==1)

}





