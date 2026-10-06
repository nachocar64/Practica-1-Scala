import scala.util.control.TailCalls.TailRec
import scala.annotation.tailrec
object Practica1 {
    // EJERCICIO 1
    def decimalABinario(num: Int): String = {
        if num == 1 then s"${1%2}"
        else s"${decimalABinario(num/2)}${num%2}"
    }

    // EJERCICIO 2
    def max(a: Int, b: Int): Int = {
        if a > b then a else b
    }

    def digitoMayor(num: Int): Int = {
        if num < 10 then num
        else max(num%10, digitoMayor(num/10))
    }
    
    // EJERCICIO 3
    def contarDigitosPares(num: Int): Int = {
        if num < 10 then {
            if num % 2 == 0 then 1
            else 0
        } else if num % 2 == 0 then contarDigitosPares(num/10) + 1
        else contarDigitosPares(num/10) 
    }

    //  EJERCICIO 4
    def contarDigito(num: Int, digit: Int): Int = {
        if num < 10 then {
            if num == digit then 1
            else 0
        } else if num % 10 == digit then contarDigito(num/10, digit) + 1
        else contarDigito(num/10, digit)
    }

    // EJERCICIO 5
    def extraerPares(num: Int): String = {
        if num < 10 then {
            if num % 2 == 0 then s"$num"
            else ""
        } else if num % 2 == 0 then extraerPares(num/10) + s"${num%10}"
        else extraerPares(num/10)
    }

    // RECURSIVIDAD DE COLA
    // EJERCICIO 6
    @tailrec
    def sumarDigitosPares(num: Int, acum: Int = 0): Int = {
        if num < 10 then {
            if num % 2 == 0 then acum + num
            else acum
        } else if num % 2 == 0 then sumarDigitosPares(num/10, acum + num%10)
        else sumarDigitosPares(num/10, acum)
    }

    // EJERCICIO 7
    // me di la libertad de buscar si ya existia un método para ver si un char era mayúscula o no (el isUpper)
    @tailrec
    def contarMayusculas(texto: String, acum: Int = 0): Int = {
        if texto.length() == 0 then acum
        else if texto.head.isUpper then contarMayusculas(texto.tail, acum + 1)
        else contarMayusculas(texto.tail, acum)
    }

    // EJERCICIO 8
    @tailrec
    def contarCambiosParidad(num: Int, acum: Int = 0): Int = {
        val ultDigit: Int = num % 10
        val penultDigit: Int = (num/10)%10
        if num < 10 then acum
        else if ultDigit % 2 == 0 then {
            if penultDigit % 2 != 0 then contarCambiosParidad(num/10, acum + 1)
            else contarCambiosParidad(num/10, acum)
        } else if penultDigit % 2 == 0 then{
            if ultDigit % 2 != 0 then contarCambiosParidad(num/10, acum + 1)
            else contarCambiosParidad(num/10, acum)
        } else contarCambiosParidad(num/10, acum)
    }

    // EJERCICIO 9
    @tailrec
    def segundoMayorDigito(num: Int, mayor: Int = -1, acum: Int = 0): Int = {
        val ultDigit: Int = num % 10
        if num == 0 then acum 
        else if ultDigit == mayor || ultDigit == acum then segundoMayorDigito(num/10, mayor, acum)
        else if ultDigit > mayor then segundoMayorDigito(num/10, ultDigit, mayor)
        else if ultDigit > acum && ultDigit < mayor then segundoMayorDigito(num/10, mayor, ultDigit) 
        else segundoMayorDigito(num/10, mayor, acum) 
    }

    // EJERCICIO 10
    @tailrec
    def rachaMaxima(texto: String, racha: Int = 1, mejor: Int = 0): Int = {
        if texto.length() == 0 then mejor
        else if texto.length() == 1 then max(racha, mejor)
        else if texto.head == texto.tail.head then rachaMaxima(texto.tail, racha + 1, max(racha + 1, mejor))
        else rachaMaxima(texto.tail, 1, max(racha, mejor))
    }

    def main (args: Array[String]): Unit = {
        println("----EJERCICIO 1----")
        println(decimalABinario(1))
        println(decimalABinario(2))
        println(decimalABinario(5))
        println(decimalABinario(10)) 
        println(decimalABinario(25)) 
        println(decimalABinario(42))
        println("----EJERCICIO 2----")
        println(digitoMayor(58329))
        println(digitoMayor(4441))
        println(digitoMayor(123456))
        println(digitoMayor(80723))
        println(digitoMayor(5))
        println("----EJERCICIO 3----")
        println(contarDigitosPares(583246))
        println(contarDigitosPares(13579))
        println(contarDigitosPares(2468))
        println(contarDigitosPares(102030))
        println(contarDigitosPares(7))
        println("----EJERCICIO 4----")
        println(contarDigito(525235,5))
        println(contarDigito(11111,1))
        println(contarDigito(987654,3))
        println(contarDigito(707070,0))
        println("----EJERCICIO 5----")
        println(extraerPares(583246))
        println(extraerPares(13579))
        println(extraerPares(2468))
        println(extraerPares(102034))
        println(extraerPares(908172))
        println("----EJERCICIO 6----")
        println(sumarDigitosPares(583246))
        println(sumarDigitosPares(13579))
        println(sumarDigitosPares(2468))
        println(sumarDigitosPares(102030))
        println(sumarDigitosPares(8))
        println("----EJERCICIO 7----")
        println(contarMayusculas("HolaMundoScala"))
        println(contarMayusculas("SCALA"))
        println(contarMayusculas("programacion"))
        println(contarMayusculas("Scala3EsGenial"))
        println(contarMayusculas(""))
        println("----EJERCICIO 8----")
        println(contarCambiosParidad(123456))
        println(contarCambiosParidad(248135))
        println(contarCambiosParidad(2468))
        println(contarCambiosParidad(13579))
        println(contarCambiosParidad(52841))
        println("----EJERCICIO 9----")
        println(segundoMayorDigito(58329))
        println(segundoMayorDigito(7416))
        println(segundoMayorDigito(9995))
        println(segundoMayorDigito(777))
        println(segundoMayorDigito(908090))
        println(segundoMayorDigito(74))
        println("----EJERCICIO 10----")
        println(rachaMaxima("aaabbccccdaa"))
        println(rachaMaxima("aabbbbbcc"))
        println(rachaMaxima("abcde"))
        println(rachaMaxima("aaaaaa"))
        println(rachaMaxima("aabbbaaaa"))
        println(rachaMaxima(""))
        println(rachaMaxima("aaAAaa"))
    }
}