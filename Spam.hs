module Spam where

-------- Supuestos-------
--Los valores trabajo son uniformes, en general toda la información se supone que es uniforme.
--Los valores son precisos y no hay nada extraordinario
-- El riesgo no puede ser negativo y no debe superar el 20%
--No hay impuestos adicionales
-- Todos los valores son en la misma moneda y no hay conversiones
-- No se asume inflación



-- El proyecto en la suma de valores que se separan por separado
-- Por la naturaleza del paradigma se DEBE separar cada uno de los proceos; se podría ver como un tubo que va pasando valores a otros tubos
-- Por esto las funciones toman valores iniciales, con los cuales operan y devuelven un resultado, este resultado es usado por otra función para generar otro resultado
-- Cuando este proceso llega al final se suman los resultados finales dandonos así nuestro resultado total, el cual es la suma de resultados derivados de otros procesos
-- Con esto la programación funcional te obliga definir estrictamente los tipos, los parametros y retornos de las funciones.
-- Luego, hay que juntar esas funcions para dar un resultado final
-- Esta naturaleza hace que cada programa sea MUY flexible, pues la unica conexión entre funciones son sus resultados
-- En nada importa como se llegaron a ellos. Así que si un día se necesita cambiar algo en una función, siempre y cuando retorne lo mismo, nada importará en el resto de funciones


data Esfuerzo = Esfuerzo
    {   valorHora :: Double
    ,   horasTotales :: Double
    ,   numeroPersonas :: Double
    } deriving (Show, Eq)

data Infraestructura = Infraestructura
    { costoNube :: Double
    ,   github :: Double
    ,   swagger :: Double
    ,   sonar ::Double
    ,   otros :: Double
    } deriving(Show, Eq)

data Costos = Costos
    { esfuerzo :: Esfuerzo
    , viaticos :: Double
    , infraestructura :: Infraestructura
    } deriving(Show, Eq)

data Gastos = Gastos
    { papeleria :: Double
    , servicios :: Double
    , internet :: Double
    , energia :: Double
    , otrosGastos :: Double
    } deriving (Show, Eq)

data Impuestos = Impuestos
    { retencionFuente :: Double  -- 11% sobre base gravable
    , reteica :: Double         -- 1% sobre retención en la fuente
    , iva :: Double            -- 19% sobre base + retenciones
    } deriving (Show, Eq)

data ResultadoConsultoria = ResultadoConsultoria
    { totalCostos :: Double
    , totalGastos :: Double
    , totalRiesgos :: Double
    , totalImpuestos :: Impuestos
    , valorTotal :: Double
    } deriving (Show, Eq)


calcularEsfuerzo :: Esfuerzo -> Double
calcularEsfuerzo (Esfuerzo valorH horasT numeroPersonas) = valorH * horasT * numeroPersonas

calcularInfra :: Infraestructura -> Double
calcularInfra (Infraestructura nube git swagger sonar otros) = nube + git + swagger + sonar + otros

calcularTotCos :: Costos -> Double
calcularTotCos (Costos esfuerzo viaticos infra) = calcularEsfuerzo esfuerzo + viaticos + calcularInfra infra

calcularTotalGastos :: Gastos -> Double
calcularTotalGastos (Gastos papel serv inet energ otros) = papel + serv + inet + energ + otros

-- validación de riegos, validar si tiene sentido el input
validarRiesgos :: Double -> Double -> Either String Double
validarRiesgos riesgos baseCalculo
    | porRiesgo > 20 = Left "Mucho riesgo"
    | riesgos < 0 = Left "No hay riesgos negativos"
    | otherwise = Right riesgos
    where porRiesgo = (riesgos / baseCalculo) * 100

-- Calcular impuestos
calcularImpuestos :: Double -> Double -> Double -> Impuestos
calcularImpuestos costos gastos riesgos =
    let baseGravable = costos + gastos + riesgos
        retencionF = baseGravable * 0.11
        retencionVal = retencionF * 0.01
        baseIVA = baseGravable + retencionF + retencionVal
        ivaVal = baseIVA * 0.19
    in Impuestos retencionF retencionVal ivaVal

-- Función principal
calcularConsultoria :: Costos -> Gastos -> Double -> Either String ResultadoConsultoria
calcularConsultoria costosData gastosData riesgos = do
    let totalCosto = calcularTotCos costosData
        totalGasto = calcularTotalGastos gastosData
        baseValidacion = totalCosto + totalGasto
    -- Validación riesgo
    riesgosValidados <- validarRiesgos riesgos baseValidacion

    let impuestos = calcularImpuestos totalCosto totalGasto riesgosValidados
        totalImpuestosVal = retencionFuente impuestos + reteica impuestos + iva impuestos
        valorTotalFinal = totalCosto + totalGasto + riesgosValidados + totalImpuestosVal

    -- Elimina gananciaEsperada para que ResultadoConsultoria reciba solo 5 argumentos
    return $ ResultadoConsultoria { totalCostos = totalCosto, totalGastos = totalGasto, totalRiesgos = riesgosValidados, totalImpuestos = impuestos,
     valorTotal = valorTotalFinal}


