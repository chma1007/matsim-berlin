# ===== MATSim 批量分析脚本（支持 basic / maut / park；自动匹配文件名与列名） =====
# 输出目录：D:/berlin/analysis/out

# ---------- 0) 依赖 ----------
need <- c("tidyverse","matsim")
inst <- need[!need %in% rownames(installed.packages())]
if (length(inst)) install.packages(inst, dependencies = TRUE)
suppressPackageStartupMessages({
  library(tidyverse)
  library(matsim)
})

# ---------- 1) 场景输出路径 ----------
paths <- list(
  basic = "D:/berlin/output/berlin-v6.4-1pct",
  maut  = "D:/berlin/output/berlin-v6.4-1pct-maut",
  park  = "D:/berlin/output/berlin-v6.4-1pct-parking"
)

# MoneyEvents 文件（固定路径）
money_files <- list(
  maut = "D:/berlin/output/berlin-v6.4-1pct-maut/berlin-v6.4.toll_events.tsv",
  park = "D:/berlin/output/berlin-v6.4-1pct-parking/berlin-v6.4.output_personMoneyEvents/berlin-v6.4.output_personMoneyEvents.tsv"
)

# ---------- 2) 工具函数 ----------
# trips 读取（分号分隔）
read_trips <- function(p) {
  cand <- list.files(p, pattern = "output_trips.*\\.csv(\\.gz)?$", full.names = TRUE, ignore.case = TRUE)
  if (length(cand) == 0) stop("找不到 trips 文件：", p)
  readr::read_delim(cand[1], delim = ";", show_col_types = FALSE)
}

# scoreStats 读取（分号分隔）
read_scores <- function(p) {
  cand <- list.files(p, pattern = "^scoreStats\\.csv$", full.names = TRUE, ignore.case = TRUE)
  if (length(cand) == 0) stop("找不到 scoreStats 文件：", p)
  readr::read_delim(cand[1], delim = ";", show_col_types = FALSE)
}

# money TSV 读取（金额取正）
read_money_tsv <- function(file) {
  if (!file.exists(file)) stop("找不到 money TSV：", file)
  df <- readr::read_tsv(file, show_col_types = FALSE)
  cn <- names(df); lc <- tolower(cn)
  amt_idx <- match(TRUE, lc %in% c("amount","value","toll","charge"))
  if (is.na(amt_idx)) {
    idx2 <- grep("(amount|value|toll|charge)", lc)
    if (length(idx2) == 0) stop("money 文件未找到金额列")
    amt_idx <- idx2[1]
  }
  amt_col <- cn[amt_idx]
  person_candidates <- c("person","personid","id","agent","agentid")
  per_idx <- match(TRUE, lc %in% person_candidates)
  if (is.na(per_idx)) {
    tibble(total_eur = -sum(df[[amt_col]], na.rm = TRUE), avg_eur = NA_real_)
  } else {
    per_col <- cn[per_idx]
    per <- df %>% group_by(.data[[per_col]]) %>%
      summarise(paid = -sum(.data[[amt_col]], na.rm = TRUE), .groups = "drop")
    tibble(total_eur = sum(per$paid, na.rm = TRUE), avg_eur = mean(per$paid, na.rm = TRUE))
  }
}

# 找模式列
find_mode_col <- function(df) {
  cn <- names(df); lc <- tolower(cn)
  preferred <- c("mode","main_mode","leg_mode","legmode","primary_mode","primarymode","mainmode")
  idx <- which(lc %in% preferred)
  if (length(idx) == 0) idx <- grep("\\bmode\\b", lc)
  if (length(idx) == 0) stop("trips 中未找到模式列；现有列：", paste(cn, collapse = ", "))
  cn[idx[1]]
}

# 找迭代列
find_iter_col <- function(df) {
  cn <- names(df); lc <- tolower(cn)
  idx <- which(lc %in% c("iteration"))
  if (length(idx) == 0) idx <- grep("iter", lc)
  if (length(idx) == 0) stop("scoreStats 缺少 Iteration 列；现有列：", paste(cn, collapse = ", "))
  cn[idx[1]]
}

# 找平均分列
find_avg_col <- function(df) {
  cn <- names(df); lc <- tolower(cn)
  idx <- which(lc %in% c("average","avg","meanscore","averagescore","mean"))
  if (length(idx) == 0) idx <- grep("(average|mean)", lc)
  if (length(idx) == 0) stop("scoreStats 缺少 Average 列；现有列：", paste(cn, collapse = ", "))
  cn[idx[1]]
}

# ---------- 3) 分析 ----------
# 3.1 Modal Split & Modal Shift
trips_all <- purrr::imap_dfr(paths, ~ read_trips(.x) %>% mutate(scenario = .y))
mode_col <- find_mode_col(trips_all)

modal_split <- trips_all %>%
  group_by(scenario, .data[[mode_col]]) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(scenario) %>%
  mutate(share = 100 * n / sum(n)) %>%
  rename(mode = !!mode_col) %>%
  arrange(scenario, desc(share))

baseline <- modal_split %>% filter(scenario == "basic") %>% select(mode, share_baseline = share)

modal_shift <- modal_split %>%
  filter(scenario != "basic") %>%
  mutate(scenario_keep = scenario) %>%              # 保留 scenario
  left_join(baseline, by = "mode") %>%
  mutate(
    shift_pp = share - share_baseline,
    scenario = scenario_keep
  ) %>%
  select(scenario, mode, share, share_baseline, shift_pp) %>%
  arrange(scenario, desc(shift_pp))

# 3.2 Money
money <- bind_rows(
  if (file.exists(money_files$maut)) read_money_tsv(money_files$maut) %>% mutate(scenario = "maut"),
  if (file.exists(money_files$park)) read_money_tsv(money_files$park) %>% mutate(scenario = "park")
) %>% relocate(scenario)

# 3.3 Scores
scores_all <- purrr::imap_dfr(paths, ~ read_scores(.x) %>% mutate(scenario = .y))
iter_col <- find_iter_col(scores_all)
avg_col  <- find_avg_col(scores_all)

last_iter_tbl <- scores_all %>%
  group_by(scenario) %>%
  summarise(last_iter = max(.data[[iter_col]], na.rm = TRUE), .groups = "drop")

scores_last <- scores_all %>%
  inner_join(last_iter_tbl, by = c("scenario", !!iter_col := "last_iter")) %>%
  summarise(
    mean_avg = mean(.data[[avg_col]], na.rm = TRUE),
    sd_avg   = sd(.data[[avg_col]],   na.rm = TRUE),
    min_avg  = min(.data[[avg_col]],  na.rm = TRUE),
    max_avg  = max(.data[[avg_col]],  na.rm = TRUE),
    .by = scenario
  ) %>% arrange(scenario)

# ---------- 4) 导出 ----------
outdir <- "D:/berlin/analysis/out"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
write_csv(modal_split, file.path(outdir, "modal_split.csv"))
write_csv(modal_shift, file.path(outdir, "modal_shift_vs_basic.csv"))
write_csv(money,       file.path(outdir, "money_summary.csv"))
write_csv(scores_last, file.path(outdir, "scores_last_iter_summary.csv"))

message("✅ 分析完成！结果已输出到：", outdir)
