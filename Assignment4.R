frequency <- c(0.6, 0.3, 0.4, 0.4, 0.2, 0.6, 0.3, 0.4, 0.9, 0.2)
bp <- c(103, 87, 32, 42, 59, 109, 78, 205, 135, 176)
first <- c(1, 1, 1, 1, 0, 0, 0, 0, NA, 1)
second <- c(0, 0, 1, 1, 0, 0, 1, 1, 1, 1)
finalDecision <- c(0, 1, 0, 1, 0, 1, 0, 1, 1, 1)

med_df <- data.frame(frequency, bp, first, second, finalDecision)
med_df <- na.omit(med_df)

hist(med_df$bp, main="Blood pressure", xlab='bp')
boxplot(med_df$bp, xlab='Blood pressure')
mean(med_df$finalDecision)

decisions <- rowMeans(med_df[, -which(names(med_df) %in% c('frequency', 'bp'))])
hist(decisions, main="Average of decision", xlab='mean of first, second, final')
boxplot(decisions, xlab="Average of decisions")