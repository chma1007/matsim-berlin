#!/bin/bash --login
#SBATCH --job-name=berlin-base-10pct
#SBATCH --output=berlin-base-10pct.log
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=400G
#SBATCH --time=96:00:00
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=mao@campus.tu-berlin.de

module unload java
module load java/21


cd /work/mao/berlin


java -Xmx360G -Xms32G -XX:+UseG1GC \
  -cp "matsim-berlin-6.4-81c27fd-dirty.jar:target/classes:target/dependency/*" \
  org.matsim.run.OpenBerlinScenario run \
  --10pct \
  --config:controler.lastIteration=500 \
  --runId=base-10pct-500 \
  --output=output/base-10pct-500
