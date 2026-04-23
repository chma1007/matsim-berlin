#!/bin/bash --login
#SBATCH --job-name=berlin-maut-10pct
#SBATCH --output=berlin-maut-10pct.log
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=120G
#SBATCH --partition=smp
#SBATCH --time=5-00:00:00
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=mao@campus.tu-berlin.de


module purge
module load java/21

export JAVA_HOME=/afs/math/software/java/jdk21
export PATH=$JAVA_HOME/bin:$PATH

cd /net/work/mao/berlin


rm -f output/maut-10pct-500/*.lock


mvn -q clean compile -DskipTests
mvn -q dependency:copy-dependencies


java -Xmx110G -cp "target/classes:target/dependency/*" \
  org.matsim.run.RunOpenBerlinWithMaut run \
  --config=input/v6.4/berlin-v6.4-1pct-maut.config.xml \
  --10pct \
  --iterations=500 \
  --runId=maut-10pct-500 \
  --output=output/maut-10pct-500
