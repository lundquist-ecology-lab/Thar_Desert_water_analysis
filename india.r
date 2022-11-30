## Analysis of M. Weisend India data

india <- read.csv("india_data.csv", header = TRUE, stringsAsFactors = TRUE)

# Log liters of water vs # of animals
plot(log(liters) ~ animals, data = india)
model_1 <- lm(log(liters) ~ animals, data = india)
summary(model_1)
abline(model_1)

# Log liters vs # of family members
par(mar=c(6,6,5,5))
plot(log(liters) ~ family, data = india,
		ylim = c(3,8), ylab = expression(Log ~ liters ~ of ~ water ~ day^-1), 
		xlab = "Number of family members",
		col = "black", pch = 16, cex = 2, cex.lab = 2, cex.axis = 2)
model_2 <- lm(log(liters) ~ family, data = india)
summary(model_2)
abline(model_2, cex = 2)


# Log literes vs # of dependents (e.g., children)
plot(log(liters) ~ dependents, data = india)
model_3 <- lm(log(liters) ~ dependents, data = india)
summary(model_3)
abline(model_3)

## Is number of family members and number of dependents independent?
plot(family ~ dependents, data = india)
model_ind <- lm(family ~ dependents, data = india)
summary(model_ind)
abline(model_ind)
## No, they are not independent


multi.model.1 <- lm(log(liters) ~ dependents + animals, data = india)
multi.model.2 <- lm(log(liters) ~ family + animals, data = india)
multi.model.3 <- lm(log(liters) ~ dependents, data = india)
multi.model.4 <- lm(log(liters) ~ family, data = india) ## Best fit to data ***
multi.model.5 <- lm(log(liters) ~ 1, data = india)

AIC(multi.model.1, multi.model.2, multi.model.3, multi.model.4, multi.model.5)

## ANCOVA

ancova_model_1 <- aov(log(liters) ~ family + pain + gs + affluence + hl, data = india)
formula(step(ancova_model_1))

ancova_model_2 <- aov(log(liters) ~ family + hl, data = india)

anova(ancova_model_2)
