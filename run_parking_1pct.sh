#!/bin/bash --login
#SBATCH --job-name=berlin-parking-1pct
#SBATCH --output=berlin-parking-1pct.log
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#SBATCH --mem=160G
#SBATCH --partition=smpshort
#SBATCH --time=2-00:00:00
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=1@campus.tu-berlin.de

module purge
module load java/21

cd /net/work/mao/berlin

mvn -q clean install \
  -DskipTests \
  -Dcheckstyle.skip=true \
  -Dgit-commit-id.skip=true

mvn -q dependency:copy-dependencies

mvn -q exec:java \
  -Dexec.mainClass=org.matsim.run.RunOpenBerlinWithParking \
  -Dexec.args="run \
    --config=input/v6.4/berlin-v6.4-1pct-parking.config.xml \
    --iterations=500 \
    --runId=parking-1pct-500 \
    --output=output/parking-1pct-500it"

