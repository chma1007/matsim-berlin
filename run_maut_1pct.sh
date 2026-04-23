#!/bin/bash --login
#SBATCH --job-name=berlin-maut-1pct
#SBATCH --output=berlin-maut-1pct.log
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8       
#SBATCH --mem=96G                
#SBATCH --partition=smpshort
#SBATCH --time=2-00:00:00
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=0@tu-berlin.de

module purge
module load java/21
cd /net/work/mao/berlin


mvn -q clean compile -DskipTests
mvn -q dependency:copy-dependencies

java -Xmx88G -cp "target/classes:target/dependency/*" \
  org.matsim.run.RunOpenBerlinWithMaut run \
  --config=input/v6.4/berlin-v6.4-1pct-maut.config.xml \
  --iterations=500 \
  --runId=maut-1pct-500 \
  --output=output/maut-1pct-500itsimpleversion
