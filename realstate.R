library(tidyverse)
library(modelr)

df_propiedades <- read_csv("realstate.csv")

df_propiedades_2 <- df_propiedades %>% select(fecha_publicacion = start_date, fecha_fin_publicacion = end_date, l2, distrito = l3, ambientes = rooms, habitaciones = bedrooms, baños = bathrooms, superficie_total = surface_total, superficie_cubierta = surface_covered, precio = price, tipo_propiedad = property_type)

df_propiedades_agrupado <- df_propiedades_2 %>% group_by(l2) %>% count()

df_propiedades_2 <- filter(df_propiedades_2, tipo_propiedad %in% c("Departamento","PH","Casa"))

glimpse(df_propiedades_2)

df_propiedades_2 <- mutate(df_propiedades_2, ambientes= as.integer(ambientes), habitaciones = as.integer(habitaciones), baños = as.integer(baños), superficie_total = as.integer(superficie_total), superficie_cubierta = as.integer(superficie_cubierta), precio = as.integer(precio))

df_propiedades_2 <- filter(df_propiedades_2, ambientes <= 10, precio <=700000, baños <= 6, habitaciones <= 10)

glimpse(df_propiedades_2)

##labeldf<-group_by(df_propiedades_2, ) Poner etiquetas a los notches paa comprar efectivamente las medianas

ggplot(df_propiedades_2, aes(x = tipo_propiedad, y = precio, fill = tipo_propiedad)) +
  geom_boxplot(notch = T) 

df_propiedades_3 <- filter(df_propiedades_2, superficie_cubierta <= superficie_total, superficie_cubierta >= 20)
df_propiedades_3 <- filter(df_propiedades_3, superficie_cubierta < 1000)
## Para quedarnos con datos lógicos filtramos las propiedades. En primer lugar que la superficie cubeirta sea menor o iguala a la total, y que la superficie cubierta sea mayor o igual a 20 y menor a 1000.
## Para tomar esta decisión estuvimos viendo precios reales de las propiedades y sus relaciones de tamaño


df_propiedades_3$relacion_superficie= df_propiedades_3$superficie_cubierta / df_propiedades_3$superficie_total
## Creamos una variable llamda realcion_superficie que relaciona la cantidad de m2 cubiertos con los totales. 
## Decidimos dividir los m2 cubiertos por los totales para que nos quede un valor menor o igual a uno

##La idea es hacer modelos para predecir los valores de las propiedades teniendo en cuenta la variable qe creamos
##la ubicación, y el tipo de propiedad. Para lograrlo vamos a separar el dataset en las 4 grandes zonas "l2"
##plotear un scatter con los precios agregando un facet wrap en relación al tipo de propiedad 

unique(df_propiedades_3$l2)

df_propiedades_CF <-filter(df_propiedades_3, l2=="Capital Federal")
df_propiedades_GBAZN <-filter(df_propiedades_3, l2=="Bs.As. G.B.A. Zona Norte")
df_propiedades_GBAZS <-filter(df_propiedades_3, l2=="Bs.As. G.B.A. Zona Sur")
df_propiedades_GBAZO <-filter(df_propiedades_3, l2=="Bs.As. G.B.A. Zona Oeste")



ggplot(df_propiedades_GBAZN, aes(y=precio, color=tipo_propiedad)) +
  geom_point(aes(x=superficie_cubierta))+
  facet_wrap(~distrito)

ggplot(df_propiedades_GBAZS, aes(y=precio, color=tipo_propiedad)) +
  geom_point(aes(x=superficie_cubierta))+
  facet_wrap(~distrito)

ggplot(df_propiedades_GBAZO, aes(y=precio, color=tipo_propiedad)) +
  geom_point(aes(x=superficie_cubierta))+
  facet_wrap(~distrito)

##Quisimos usar la variable que creamos pero no vimos mucha relación, por eso decidimos usar el precio
## en relación a la superficie cubierta, coloreamos por tip de propiedad e hicimos el facet wrap por el distrito(Barrios o partidos)
## Se ve la tendencia que esperabamos, a mayor superficie, mayor precio, la pendiente varía bastante según la zona que
## también es lo que esperabamos. Concluimos que la superficie cubierta puede ser una buena variable para el modelo

barrios_comunas <- data.frame(
  barrio = c('Retiro', 'San Nicolás', 'Puerto Madero', 'San Telmo', 'Monserrat', 'Constitución',
             'Recoleta', 'Balvanera', 'San Cristobal', 'Boca', 'Barracas', 'Parque Patricios', 
             'Nueva Pompeya', 'Almagro', 'Boedo', 'Caballito', 'Flores', 'Parque Chacabuco', 
             'Villa Soldati', 'Villa Riachuelo', 'Villa Lugano', 'Liniers', 'Mataderos', 
             'Parque Avellaneda', 'Villa Real', 'Monte Castro', 'Versalles', 'Floresta', 
             'Velez Sarsfield', 'Villa Luro', 'Villa General Mitre', 'Villa Devoto', 
             'Villa del Parque', 'Villa Santa Rita', 'Coghlan', 'Saavedra', 'Villa Urquiza', 
             'Villa Pueyrredón', 'Belgrano', 'Nuñez', 'Colegiales', 'Palermo', 'Chacarita', 
             'Villa Crespo', 'Paternal', 'Villa Ortuzar', 'Agronomía', 'Parque Chas',
             'Abasto', 'Barrio Norte', 'Catalinas', 'Centro / Microcentro', 'Congreso', 
             'Las Cañitas', 'Once', 'Parque Centenario', 'Pompeya', 'Tribunales'),
  comuna = c("1", "1", "1", "1", "1", "1", "2", "3", "3", "4", "4", "4", "4", "5", "5", "6", "7", "7", "8", "8", "8", "9", "9", "9", 
             "10", "10", "10", "10", "10", "10", "11", "11", "11", "11", "12", "12", "12", "12", "13", "13", "13", "14", "15", 
             "15", "15", "15", "15", "15", "3", "2", "1", "1", "1", "14", "3", "15", "4", "1")
)

df_propiedades_CF <- df_propiedades_CF %>%
  left_join(barrios_comunas, by = c("distrito" = "barrio"))


modCF <-lm(df_propiedades_CF, formula= precio ~ superficie_cubierta)
summary(modCF)




grillaCF <- data_grid(df_propiedades_CF, precio= seq_range(superficie_cubierta, n=100))
df_propiedades_CF <- add_predictions(df_propiedades_CF, modCF)

ggplot(df_propiedades_CF, aes(x=superficie_cubierta, y=precio, color=tipo_propiedad)) +
  geom_point(aes(x=superficie_cubierta, y=precio))+
  geom_line(data= df_propiedades_CF, 
            aes(y=pred),
            color="black")+
  facet_wrap(~distrito)

df_propiedades_CF <- add_residuals(df_propiedades_CF, modCF)
plot(modCF)

##Vimos una tendencia en los residuos, procedemos a agregar variables al modelo para tratar de explicarla

modCF <-lm(df_propiedades_CF, formula= precio ~ poly(superficie_cubierta, 2))
summary(modCF)
grillaCF <- data_grid(df_propiedades_CF, precio= seq_range(superficie_cubierta, n=25))
df_propiedades_CF <- add_predictions(df_propiedades_CF, modCF)

ggplot(df_propiedades_CF, aes(x=superficie_cubierta, y=precio, color=tipo_propiedad)) +
  geom_point(aes(x=superficie_cubierta, y=precio))+
  geom_line(data= df_propiedades_CF, 
            aes(y=pred),
            color="black")+
  facet_wrap(~distrito)

df_propiedades_CF <- add_residuals(df_propiedades_CF, modCF)
plot(modCF)

##Probamos con GBAZS

modGBAZS <-lm(df_propiedades_GBAZS, formula= precio ~ poly(superficie_cubierta, 2))
summary(modGBAZS)
grillaGBZS <- data_grid(df_propiedades_GBAZS, precio= seq_range(superficie_cubierta, n=25))
df_propiedades_GBAZS <- add_predictions(df_propiedades_GBAZS, modGBAZS)

ggplot(df_propiedades_GBAZS, aes(x=superficie_cubierta, y=precio, color=tipo_propiedad)) +
  geom_point(aes(x=superficie_cubierta, y=precio))+
  geom_line(data= df_propiedades_GBAZS, 
            aes(y=pred),
            color="black")+
  facet_wrap(~distrito)

df_propiedades_GBAZS <- add_residuals(df_propiedades_GBAZS, modGBAZS)
plot(modGBAZS)
##En que ubicación conviene comprar una propiedad?
## PH, Depto o Casa: Presentar la distribución en barras por zona 
## Cantidad de ambientes /M2: Usar los modelos para sacar precios promedio y explicar como influyen en el precio final


##Gráfico de barras de la distribución de propiedades
ggplot(data=df_propiedades_3) +
  geom_bar(mapping = aes(x=tipo_propiedad, fill = l2), position = "dodge")
##Esto ya nos da una idea para guía posibles "clientes". Se puede estudiar la distribución de cada zona en particular
## y tal vez hacer modelos más específicos todavía, o pasarle datos mejor agrupados al mismo modelo. Tal vez los baños
## y los ambientes cambien mucho el valor dentro de los departamentos que son la mayoría de los datos que tenemos


ggplot(data=df_propiedades_CF) +
  geom_bar(mapping = aes(x=tipo_propiedad, fill = distrito), position = "dodge")
##Para capital son muchos barrios, no se puede leer correctamente. Habría que separar en comunas 



##Grafico de barras para ver distribucion de propiedades en las distintas comunas de CABA
ggplot(data = df_propiedades_CF) +
  geom_bar(mapping = aes(x = comuna , fill = tipo_propiedad), position = "dodge") +
  labs(x = "Comuna", y = "Cantidad de propiedades", fill = "Tipo de propiedad", title = "Cantidad de propiedades por comuna", subtitle = "Desagregado por el tipo de propiedad") +
  theme(axis.title.y = element_text(size = 16)) +
  theme(legend.title = element_text(size = 16)) +
  theme(plot.title = element_text(size = 24, face = "bold")) +
  theme(plot.subtitle = element_text(size = 20)) +
  theme(axis.text = element_text(size = 12))
