#!/bin/bash --login
#SBATCH --job-name=berlin-base-1pct
#SBATCH --output=berlin-base-1pct.log
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=12
#SBATCH --mem=200G
#SBATCH --time=48:00:00
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=你的邮箱@tu-berlin.de

module unload java
module load java/21
cd /work/mao/berlin

# 运行 base (1pct, 500 iterations)
java -Xmx180G -cp "target/matsim-berlin-6.4.jar:target/dependency/*" \
  org.matsim.run.OpenBerlinScenario run \
  --1pct \
  --config:controler.lastIteration=500 \
  --runId=base-1pct-500 \
  --output=output/base-1pct-500

