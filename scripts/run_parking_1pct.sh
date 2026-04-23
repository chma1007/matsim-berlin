#!/bin/bash --login
#SBATCH --job-name=berlin-parking-1pct
#SBATCH --output=berlin-parking-1pct.log
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#SBATCH --mem=160G
#SBATCH --time=48:00:00
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=你的邮箱@tu-berlin.de

module unload java
module load java/21

cd /work/mao/berlin

# 如果需要重新打包项目可以取消注释
# mvn clean install -DskipTests
# mvn dependency:copy-dependencies

java -Xmx140G -cp "target/matsim-berlin-6.4.jar:target/dependency/*" \
  org.matsim.run.RunOpenBerlinWithParking run \
  --config=/net/work/mao/berlin/input/v6.4/berlin-v6.4-1pct-parking.config.xml \
  --iterations=500 \
  --runId=parking-1pct-500 \
  --output=output/parking-1pct-500
