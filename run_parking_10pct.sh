#!/bin/bash --login
#SBATCH --job-name=berlin-parking-10pct
#SBATCH --output=berlin-parking-10pct.log
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#SBATCH --mem=160G
#SBATCH --partition=smp
#SBATCH --time=5-00:00:00
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=mao@campus.tu-berlin.de

module purge
module load java/21

cd /net/work/mao/berlin

rm -f output/parking-10pct-500/*.lock

export JAVA_HOME=/afs/math/software/java/jdk21
export PATH=$JAVA_HOME/bin:$PATH

mvn -q clean compile -DskipTests
mvn -q dependency:copy-dependencies

java -Xmx150G -cp "target/classes:target/dependency/*" \
  org.matsim.run.RunOpenBerlinWithParking run \
  --config=input/v6.4/berlin-v6.4-1pct-parking.config.xml \
  --10pct \
  --iterations=500 \
  --runId=parking-10pct-500 \
  --output=output/parking-10pct-500
