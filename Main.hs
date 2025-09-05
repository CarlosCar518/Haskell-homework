module Main where
import Spam

main :: IO ()
main = do
    let esf = Esfuerzo 50 100 2
        infra = Infraestructura 200 100 50 80 20
        costos = Costos esf 300 infra
        gastos = Gastos 50 100 60 40 30

    print (calcularConsultoria costos gastos 500)

