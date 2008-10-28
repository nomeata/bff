module StatsDef where

type DataPoint = (Int, Double, Double)
type StatRunData = [DataPoint]
type StatRuns = [(String, StatRunData)]
