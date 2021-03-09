library(shiny)
library(shinythemes)
rm(list=ls())
library(tidyverse)
library(foreign)
library(questionr)
library(ggtext)

load("www/c.Rda")

# Violencia Física y psico ------------------------------------------------
VioFisicaPareja<-colnames(c%>%select(starts_with("M1P4_"),
                                     starts_with("M1P5_"),
                                     starts_with("M2P4_"),
                                     starts_with("M2P5_")))

VioPsicoPareja<-colnames(c%>%select(starts_with("M1P1_"),
                                    starts_with("M1P3_"),
                                    starts_with("M1P6"),
                                    starts_with("M2P1_"),
                                    starts_with("M2P3_"),
                                    starts_with("M2P6")))

VioFueraPareja<-colnames(c%>%select(starts_with("M3P1"),
                                    starts_with("M3P2")))

VioAcosoSexual<-colnames(c%>%select(starts_with("M4P1")))

VioAcosoRepetido<-colnames(c%>%select(starts_with("M5P1")))

Items<-colnames(c)[1:245]

vpi<-function(items=Items, Espanya=0,Sexo=0,ParejaEspanya=0, Momento="En cualquier momento de su vida"){
  
  if(Espanya==1){
    #Eliminamos agresiones fuera de Espanya
    c[which(c$EspanaM3P2G=="En el extranjero"),which(colnames(c)%in%colnames(c%>%select(starts_with("M3P2"))))]<-0
  }
  
  if(Sexo==1){
    #Quitamos mujeres pareja actual
    c[which(c$SexoPareja=="Mujer"),which(colnames(c)%in%colnames(c%>%select(starts_with("M1P1_"),
                                                                            starts_with("M1P3_"),
                                                                            starts_with("M1P4_"),
                                                                            starts_with("M1P5_"),
                                                                            M1P6, M1P6A)))]<-0
    #Quitamos mujeres parejas antiguas
    c[which(c$SexoM2P3F=="Solo mujeres"),which(colnames(c)%in%colnames(c%>%select(starts_with("M2P3_"))))]<-0
    c[which(c$SexoM2P4F=="Solo mujeres"),which(colnames(c)%in%colnames(c%>%select(starts_with("M2P4_"))))]<-0
    c[which(c$SexoM2P5F=="Solo mujeres"),which(colnames(c)%in%colnames(c%>%select(starts_with("M2P5_"))))]<-0
    #Quitamos mujeres fuera pareja
    c[which(c$SexoM3P1D=="Solo mujeres"),which(colnames(c)%in%colnames(c%>%select(starts_with("M3P1"))))]<-0
    c[which(c$SexoM3P2D=="Solo mujeres"),which(colnames(c)%in%colnames(c%>%select(starts_with("M3P2"))))]<-0
    #Quitamos mujeres acoso Sexual
    c[which(c$SexoM4P1D=="Solo mujeres"),which(colnames(c)%in%colnames(c%>%select(starts_with("M4P1"))))]<-0
    #Quitamos mujeres Stalking
    c[which(c$SexoM5P1E=="Solo mujeres"),which(colnames(c)%in%colnames(c%>%select(starts_with("M5P1"))))]<-0
  }
  
  
  if(ParejaEspanya==1){
    c[which(c$PaisNacimiento!="España"),which(colnames(c)%in%colnames(c%>%select(starts_with("M1P1_"),
                                                                                 starts_with("M1P3_"),
                                                                                 starts_with("M1P4_"),
                                                                                 starts_with("M1P5_"),
                                                                                 M1P6, M1P6A)))]<-0}
 

#Seleccionamos todos lo items (para cualquier periodo)
itemsA<-items%>%str_replace_all(c("_0_"="_A_",
                  "M1P6"="M1P6A",
                  "M2P6"="M2P6A"))
for(i in 3:5){
  for(j in 1:2){
    itemsA<-gsub(paste("\\bM",i,"P",j,"\\w+", sep=""), paste("M",i,"P",j,"A", sep=""), itemsA)
  }
}

itemsB<-itemsA%>%str_replace_all(c("_A_"="_B_",
                                  "M3P1A"="M3P1B",
                                  "M3P2A"="M3P2B",
                                  "M4P1A"="M4P1B",
                                  "M4P2A"="M4P2B",
                                  "M5P1A"="M5P1B",
                                  "M5P2A"="M5P2B"))

items<-unique(c(items, itemsA, itemsB))

if(Momento=="En los últimos 12 meses"){
    items<-items[which(grepl("A", items, fixed = TRUE)==TRUE)]
    }else if(Momento=="En los últimos 4 años"){
    items<-items[c(which(grepl("B", items, fixed = TRUE)==TRUE),which(grepl("A", items, fixed = TRUE)==TRUE))]
    } else{items<-items[-c(which(grepl("B", items, fixed = TRUE)==TRUE),which(grepl("A", items, fixed = TRUE)==TRUE))]}
  
  
  B<-c%>%mutate(Identificador=1:dim(c)[1])%>%
    select(PESO,Identificador,all_of(items))
  
  B[is.na(B)]<-0
  

  
  D<-B%>%gather(3:(dim(B)[2]), key="Tipo Vio", value="Sufrido")
  
  D<-D%>%mutate(ViolenciaFisicaySexual=ifelse(Sufrido==1&(`Tipo Vio`%in%VioFisicaPareja),1,0),
                ViolenciaPsicologica=ifelse(Sufrido==1&(`Tipo Vio`%in%VioPsicoPareja),1,0),
                ViolenciaFueraPareja=ifelse(Sufrido==1&(`Tipo Vio`%in%VioFueraPareja),1,0),
                ViolenciaAcosoSexual=ifelse(Sufrido==1&(`Tipo Vio`%in%VioAcosoSexual),1,0),
                ViolenciaAcosoRepetido=ifelse(Sufrido==1&(`Tipo Vio`%in%VioAcosoRepetido),1,0))
  
  
  
  E<-D%>%group_by(Identificador)%>%dplyr::summarise(FisicaSexualPareja=ifelse(sum(ViolenciaFisicaySexual)>0,1,0),
                                                    Psicologica=ifelse(sum(ViolenciaPsicologica)>0,1,0),
                                                    FueraPareja=ifelse(sum(ViolenciaFueraPareja)>0,1,0),
                                                    AcosoSexual=ifelse(sum(ViolenciaAcosoSexual)>0,1,0),
                                                    AcosoRepetido=ifelse(sum(ViolenciaAcosoRepetido)>0,1,0),
                                                    PESO=mean(as.numeric(PESO)))
  
  
  E<-E%>%mutate(Cantidad=FisicaSexualPareja+Psicologica+FueraPareja+AcosoSexual+AcosoRepetido)
  
  E<-E%>%mutate(Desgranado=ifelse(FisicaSexualPareja==1&Psicologica==0&FueraPareja==0&AcosoSexual==0&AcosoRepetido==0,"Solo Violencia Física/Sexual (ex)Parejas",
                                  ifelse(FisicaSexualPareja==0&Psicologica==1&FueraPareja==0&AcosoSexual==0&AcosoRepetido==0,"Solo Violencia Psico (ex)Parejas",
                                         ifelse(FisicaSexualPareja==0&Psicologica==0&FueraPareja==1&AcosoSexual==0&AcosoRepetido==0,"Solo Violencia Fuera Pareja",
                                                ifelse(FisicaSexualPareja==0&Psicologica==0&FueraPareja==0&AcosoSexual==1&AcosoRepetido==0,"Solo Acoso Sexual",
                                                       ifelse(FisicaSexualPareja==0&Psicologica==0&FueraPareja==0&AcosoSexual==0&AcosoRepetido==1,"Solo Acoso Repetido",
                                                              ifelse(FisicaSexualPareja==1&Psicologica==1&FueraPareja==0&AcosoSexual==0&AcosoRepetido==0,"Violencia Física/Sexual/Psico (ex)Parejas",
                                                                     ifelse(FisicaSexualPareja==0&Psicologica==0&FueraPareja==1&AcosoSexual==1&AcosoRepetido==1,"Violencia Fuera de la Pareja/Acosos",
                                                                            ifelse(FisicaSexualPareja==0&Psicologica==0&FueraPareja==0&AcosoSexual==0&AcosoRepetido==0,"Ninguna Violencia",
                                                                                   ifelse(FisicaSexualPareja==1&Psicologica==1&FueraPareja==1&AcosoSexual==1&AcosoRepetido==1,"Todas las Violencias","Otras Combinaciones"))))))))))
  #ResultadoMinisterio<-as.data.frame(round(prop.table(wtd.table(E$Desgranado, w=as.numeric(E$PESO))),4)*100)
  #save(ResultadoMinisterio, file="ResultadoMinisterio.Rda")
  
  load("www/ResultadoMinisterioIgualdad.Rda")
  ResultadoMinisterio<-ResultadoMinisterioIgualdad%>%dplyr::filter(Momentos==Momento)%>%dplyr::select(-Momentos)
  TuResultado<-as.data.frame(round(prop.table(wtd.table(E$Desgranado, w=as.numeric(E$PESO))),4)*100)
  
  Resultados<-ResultadoMinisterio%>%left_join(y = TuResultado, by="Var1")
  colnames(Resultados)<-c("Tipo", "Ministerio de Igualdad","Mi Resultado")
  Resultados<-Resultados%>%gather(key="Results",value="Freq",c(2,3))%>%mutate(Tipo=ifelse(Tipo=="Solo Violencia Física/Sexual (ex)Parejas", "Solo Violencia Física/Sexual \n (ex)Parejas",
                                              ifelse(Tipo=="Solo Violencia Psico (ex)Parejas","Solo Violencia Psico \n (ex)Parejas",
                                                     ifelse(Tipo=="Violencia Física/Sexual/Psico (ex)Parejas","Violencia Física/Sexual/Psico \n (ex)Parejas", 
                                                            ifelse(Tipo=="Violencia Fuera de la Pareja/Acosos","Violencia Fuera de\n la Pareja/Acosos",as.character(Tipo))))))  
  
  
  Resultados[is.na(Resultados)]<-0
  M<-levels(as.factor(Resultados$Tipo))
  
  Resultados$Tipo<-factor(Resultados$Tipo,levels = c(M[1],M[8],M[2],
                                                     M[10],M[4],M[3],
                                                     M[6],M[9],M[7],
                                                     M[5]) )
 
  graph1<-Resultados%>%ggplot(aes(x=Tipo,y=Freq, fill=Results))+
    geom_bar(stat="identity", alpha=0.7, position = "dodge", aes(alpha=Results))+
    geom_text(aes(label=paste(round(Freq,1),"%", sep="")), size=3.2, position=position_dodge(width = 1), hjust = 0)+
    scale_fill_manual(values = c("#96a76a","#7b6aa7"), guide = guide_legend(reverse=TRUE))+
    theme_minimal()+labs(title=paste("Porcentaje de mujeres que ha sufrido violencia",str_to_lower(Momento),sep=" "), subtitle = "(Desagregado por tipo de violencia)")+
    theme(legend.title = element_blank(),plot.subtitle = element_text(h=0.5) ,plot.title=element_text(h=0.5, size = 15, colour = "grey20"),
          panel.grid.minor.y = element_blank(),panel.grid.major.y = element_line(color = "gray", linetype = 9),panel.grid.minor.x = element_blank(),panel.grid.major.x = element_line(color = "gray", linetype = 8),
          axis.text.x = element_text(size=11, angle=90, vjust = 0.5, hjust=1),axis.text.y = element_text(size=11),axis.title.x=element_blank(),axis.title.y=element_blank(),
          text = element_text(family = "serif", size = 10),
          legend.text=element_text(size=10),
          legend.position="bottom")+coord_flip()
  

  VioMinisterio<-(100-Resultados[Resultados$Tipo=="Ninguna Violencia"&Resultados$Results=="Ministerio de Igualdad","Freq"])
  VioMio<-(100-Resultados[Resultados$Tipo=="Ninguna Violencia"&Resultados$Results=="Mi Resultado","Freq"])
  
  PrimerResultado<-data.frame(General=c(VioMio, 100-VioMio, VioMinisterio, 100-VioMinisterio),
                              Quien=c("Mi resultado", "Mi resultado", "Ministerio de Igualdad", "Ministerio de Igualdad"),
                              Violencia=c("Violencia", "No Violencia", "Violencia", "No Violencia"))
  
  
  graph0<-PrimerResultado%>%ggplot(aes(x=Quien,y=General, fill=Violencia))+
    geom_bar(stat="identity", alpha=0.7)+
    geom_text(aes(label=paste(round(General,1),"%", sep="")), size=3.2, position = position_stack(0.5))+
    scale_fill_manual(values = c("#96a76a","#7b6aa7"), guide = guide_legend(reverse=TRUE))+
    theme_minimal()+labs(title=paste("Porcentaje de mujeres que ha sufrido violencia",str_to_lower(Momento),sep=" "))+
    theme(legend.title = element_blank(),plot.title=element_text(h=0.5, size = 15, colour = "grey20"),
          panel.grid.minor.y = element_blank(),panel.grid.major.y = element_line(color = "gray", linetype = 9),panel.grid.minor.x = element_blank(),panel.grid.major.x = element_blank(),
          axis.text.x = element_text(size=11),axis.text.y = element_text(size=11),axis.title.x=element_blank(),axis.title.y=element_blank(),
          text = element_text(family = "serif", size = 10),
          legend.text=element_text(size=10),
          legend.position="bottom")
  Letra<-if(VioMinisterio>VioMio){
    paste("Aunque el Ministerio de Igualdad diga que el",VioMinisterio, "porciento de las mujeres han sufrido violencia contra la mujer",str_to_lower(Momento),", su resultado parece más matizado. Concretamente, según usted, el", round(VioMio,2), "porciento de las mujeres en España ha sufrido violencia contra la mujer", str_to_lower(Momento))
  }else{paste("Tiene usted la misma concepción de violencia que el Ministerio de Igualdad, concretamente, el", round(VioMio,2), "de las mujeres ha sufrido Violencia contra la Mujer",str_to_lower(Momento))}
  
  ret<-list(graph1, Letra, graph0)
  return(ret)
}


ui =fluidPage(theme=shinytheme("journal"),
  navbarPage(title = "Violencia contra la Mujer",
             tabPanel("Página Principal",
                      wellPanel(style = "background: #d9d4e6",
                                tags$strong("¿Qué es y para qué sirve esta página interactiva?"),
                                tags$p("En los últimos meses se ha venido", tags$a(href="https://www.lavanguardia.com/vida/20200910/483394921009/la-mitad-de-las-mujeres-en-espana-ha-sufrido-violencia-machista-11-millones.html", "diciendo"),
"que más de la mitad de las mujeres en España ha sufrido", tags$em("violencia machista*."),"Concretamente, el 57% de las españolas habrían sido víctimas",em("de la violencia contra la Mujer"), "en algún momento de su vida. Sin embargo, existe cierto debate sobre qué acciones concretas se deberían tener en cuenta para llevar a cabo los cálculos
(por ejemplo, ¿una mirada lasciva es violencia? e ¿Ignorar a tu pareja?).
Con esta página será el usuario el que elija lo que él considere más ajustado al concepto de violencia que tiene en mente, sin tener que depender del dato concreto que procede del Ministerio o medios de comunicación. Los cálculos se
       llevan a cabo con los microdatos de la ",a(href="https://violenciagenero.igualdad.gob.es/violenciaEnCifras/macroencuesta2015/Macroencuesta2019/home.htm", "Macroencuesta de Violencia Contra la Mujer 2019.")),
tags$br(),
tags$strong("¿Cómo funciona?"),
tags$p("En la parte superior de la aplicación puede encontrar distintas pestañas con las letras en lila que
       representan los distintos tipos de violencia que se extraen de la encuesta.", strong("Debe escoger los ítems que considera que deberían ser incluidos en los cálculos."),"Fíjese que al final de cada sección hay un botón para seleccionar todas las preguntas de aquél apartado. Si selecciona todas las categorías de todos los apartados obtendrá el resultado del Ministerio."),
p("Finalmente, note que en algunas secciones se especifica entre paréntesis el tipo de violencia concreta en el que pertenece cada ítem:", em("física, sexual, de control, emocional y miedo."),"Esto le servirá al usuario si quiere obtener otros resultados presentados por el Ministerio de Igualdad en su ",a(href="https://violenciagenero.igualdad.gob.es/violenciaEnCifras/macroencuesta2015/pdf/Macroencuesta_2019_estudio_investigacion.pdf","informe.")),
p("Una vez escogidos los ítems pulse el botón que tiene más abajo. Obtendrá su", strong("resultado"), "y dos", strong("gráficos"), "que le permitirá desgranar un poco más sus cálculos"), p("Por tanto, vuelva a la", strong("Página Principal"),"cuando haya escogido las variables de cada viñeta que desea tener en cuenta."),br(),p("Aplicación creada por Andreu Orestes", a(href="https://twitter.com/AndreuOrestes", "(@AndreuOrestes).")),br(),
em("*La aplicación sirve para calcular la violencia sufrida por las mujeres. No entra en las causas que la pudieran haber motivado.")),
br(),
wellPanel(tags$strong("¿Ya tiene los ítems seleccionados? Ahora marque en qué casos de los siguientes cuatro desea centrarse:"),
  style = "background: white",
  br(),
  br(),fluidRow(column(3,
                       selectInput("moment", "¿En qué momento se dieron las agresiones (cualquier tipo de violencia)?", choices = c("En los últimos 12 meses",
                                                                                                                                    "En los últimos 4 años",
                                                                                                                                    "En cualquier momento de su vida"),selected = "En cualquier momento de su vida")),
                column(3,
                sliderInput("esp", "¿Solamente agresiones ocurridas en España (solo Sexuales Fuera de la Pareja)?",
                            min = 0, max = 1, value = 0, step = 1, sep = ",", animate = FALSE, post="",ticks = F)),
                column(3,
                sliderInput("sexo", "¿Solamente agresiones realizadas por hombres (cualquier tipo de violencia)?",
                            min = 0, max = 1, value = 0, step = 1, sep = ",", animate = FALSE, post="",ticks = F)),
                column(3,
                       sliderInput("pareja", "¿Solamente agresiones realizadas en parejas actuales con nacionalidad española?",
                                   min = 0, max = 1, value = 0, step = 1, sep = ",", animate = FALSE, post="",ticks = F))),
  em("No seleccione ninguna situación si quiere llegar al resultado del Ministerio.")),
actionButton(inputId = "go", label = "Pulsa para calcular"),
br(),
br(),
                      textOutput("explicacion"),br(),plotOutput("value0"),br(),p("En cuanto al gráfico anterior, se debe tener en cuenta que
                                                                                 la selección del momento en el que se produjo la agresión no sirve para 
                                                                                 items individuales del las secciones", em("Violencia Fuera de Pareja, Acoso Sexual y Stalcking."), 
                                                                                 "El motivo es que la encuesta solo pregunta si han sufrido alguno de los items (en general) de dichas secciones
                                                                                 en los últimos 4 años o en los últimos 12 meses (hecho que no ocurre con las otras secciones).
                                                                                 Por tanto, cuando se escoge un ítem de estas tres secciones para los cálculos en los últimos 4 años o 12 meses (no ocurre para", em("cualquier momento de la vida)"),
                                                                                 "téngase en cuenta que será equivalente a escoger todos los ítems de la sección."),br(),plotOutput("value"),
p("A la hora de interpretar los resultados del gráfico anterior se debe tener en cuenta que los cálculos realizados por el Ministerio de Igualdad incluyen",strong("TODOS"),"los ítems. Por tanto, si su resultado para un tipo de violencia concreto ", em("(Solo física/Sexual (ex)parejas,"),"por ejemplo) es mayor que el del Ministerio será porque algunos individuos, que con sus cálculos únicamente caen en esta categoría, si se incluyeran otras variables podrían caer en otras categorías que engloban ésta y otros tipos de violencia como",em("Otras combinaciones"), "o",em("Todas las violencias,"), "por ejemplo."),
p("Aquí encontrará el",a(href="https://github.com/AOPB/Violencia-Mujer", "código"),"con el que se han realizado los cálculos.")),
             navbarMenu("Violencia Física y Sexual",
                        tabPanel("Violencia de Pareja",
                                 wellPanel(style = "background: #d9d4e6",
                                           checkboxGroupInput("VioFS","Violencia Física y Sexual en Parejas",
                                                                          choices=list("1.1. (Física) Le ha abofeteado o tirado algo que pudiese hacerle daño"="M1P4_0_1",
                                                                                       "1.2. (Física) Le ha empujado, agarrado o tirado del pelo"="M1P4_0_2",
                                                                                       "1.3. (Física) Le ha golpeado con su puño o con alguna otra cosa que pudiese hacerle daño"="M1P4_0_3",
                                                                                       "1.4. (Física) Le ha dado patadas, arrastrado o pegado una paliza"="M1P4_0_4",
                                                                                       "1.5. (Física) Le ha intentado asfixiar o quemar a propósito"="M1P4_0_5",
                                                                                       "1.6. (Física) Le ha amenazado con usar o ha usado una pistola, cuchillo u otra arma o substancia peligrosa contra Ud."="M1P4_0_6",
                                                                                       "1.7. (Física) Ha usado la fuerza contra usted, de cualquier otra manera de las mencionadas anteriormente, de forma que le ha hecho daño o podría haberle hecho daño"="M1P4_0_7",
                                                                                       "1.8. (Sexual) Le ha obligado a mantener relaciones sexuales amenazándole, sujetándole o haciéndole daño de alguna manera. Por relaciones sexuales queremos decir penetración vaginal o anal con pene u objetos, o sexo oral"="M1P5_0_1",
                                                                                       "1.9. (Sexual) Le ha hecho mantener relaciones sexuales cuando era incapaz de rechazarlas debido a que usted estaba bajo la influencia del alcohol o las drogas"="M1P5_0_2",
                                                                                       "1.10. (Sexual) Ha mantenido relaciones sexuales sin desearlo porque tenía miedo de lo que le podría hacer si se negaba"="M1P5_0_3",
                                                                                       "1.11. (Sexual) Le ha obligado a mantener relaciones sexuales cuando Ud. no quería"="M1P5_0_4",
                                                                                       "1.12. (Sexual) Ha intentado obligarle a tener relaciones sexuales contra su voluntad sin conseguirlo"="M1P5_0_5",
                                                                                       "1.13. (Sexual) Le ha tocado a Ud. sus partes íntimas – genitales o pecho- o le ha realizado algún otro tipo de tocamiento de tipo sexual cuando usted no quería"="M1P5_0_6",
                                                                                       "1.14. (Sexual) Le ha hecho alguna vez tocarle sus partes íntimas – genitales o pecho- o le ha obligado a realizarle algún otro tipo de tocamiento de tipo sexual cuando usted no quería"="M1P5_0_7",
                                                                                       "1.15. (Sexual) Le ha obligado a realizar alguna otra práctica de tipo sexual que yo no le haya mencionado ya"="M1P5_0_8")),
                                 actionLink("selectallFyS","Seleccionar todo"))),
                        tabPanel("Violencia de Exparejas",
                                 wellPanel(style = "background: #d9d4e6",
                                           checkboxGroupInput("VioFSEx","Violencia Física y Sexual en Exparejas",
                                                    choices=list("2.1. (Física) Le abofeteó o tiró algo que pudiese hacerle daño"="M2P4_0_1",
                                                                 "2.2. (Física) Le empujó, agarró o tiró del pelo"="M2P4_0_2",
                                                                 "2.3. (Física) Le golpeó con su puño o con alguna otra cosa que pudiese hacerle daño"="M2P4_0_3",
                                                                 "2.4. (Física) Le dio patadas, arrastró o pegó una paliza"="M2P4_0_4",
                                                                 "2.5. (Física) Le intentó asfixiar o quemar a propósito"="M2P4_0_5",
                                                                 "2.6. (Física) Le amenazó con usar o usó una pistola, cuchillo u otra arma o substancia peligrosa contra Ud."="M2P4_0_6",
                                                                 "2.7. (Física) Usó la fuerza contra usted, de cualquier otra manera de las mencionadas anteriormente, de forma que le hizo daño o podría haberle hecho daño"="M2P4_0_7",
                                                                 "2.8. (Sexual) Le obligó a mantener relaciones sexuales amenazándole, sujetándole o haciéndole daño de alguna manera. Por relaciones sexuales queremos decir penetración vaginal o anal con pene u objetos, o sexo oral"="M2P5_0_1",
                                                                 "2.9. (Sexual) Le hizo mantener relaciones sexuales cuando era incapaz de rechazarlas debido a que usted estaba bajo la influencia del alcohol o las drogas"="M2P5_0_2",
                                                                 "2.10. (Sexual) Mantuvo relaciones sexuales sin desearlo porque tenía miedo de lo que le podría hacer si se negaba"="M2P5_0_3",
                                                                 "2.11. (Sexual) Le obligó a mantener relaciones sexuales cuando Ud. no quería"="M2P5_0_4",
                                                                 "2.12. (Sexual) Intentó obligarle a tener relaciones sexuales contra su voluntad sin conseguirlo"="M2P5_0_5",
                                                                 "2.13. (Sexual) Le tocó a Ud. sus partes íntimas – genitales o pecho o le realizó algún otro tipo de tocamiento de tipo sexual cuando usted no quería"="M2P5_0_6",
                                                                 "2.14. (Sexual) Le hizo alguna vez tocarle sus partes íntimas – genitales o pecho- o le obligó a realizarle algún otro tipo de tocamiento de tipo sexual cuando usted no quería"="M2P5_0_7",
                                                                 "2.15. (Sexual) Le obligó a realizar alguna otra práctica de tipo sexual que yo no le haya mencionado ya"="M2P5_0_8")),
                                 actionLink("selectallFySEx","Seleccionar todo")))),
             navbarMenu("Violencia Psicológica",
                        tabPanel("Violencia de Pareja",
                                 wellPanel(style = "background: #d9d4e6",
                                           checkboxGroupInput("VioP","Violencia psicológica en Parejas",
                                                                          choices=list("3.1. (De Control) Trata o ha tratado de impedirle que vea a sus amigos o amigas"="M1P1_0_1",
                                                                                       "3.2. (De Control) Trata o ha tratado de evitar que Ud. se relacione con su familia directa o parientes"="M1P1_0_2",
                                                                                       "3.3. (De Control) Insiste o ha insistido en saber dónde está usted en cada momento."="M1P1_0_3",
                                                                                       "3.4. (De Control) Le ignora o ha ignorado y le trata o ha tratado con indiferencia"="M1P1_0_4",
                                                                                       "3.5. (De Control) Se enfada o se ha enfadado si habla con otro hombre o mujer"="M1P1_0_5",
                                                                                       "3.6. (De Control) Sospecha o ha sospechado sin motivos que Ud. le es/era infiel"="M1P1_0_6",
                                                                                       "3.7. (De Control) Espera o ha esperado que Ud. le pida permiso antes de ir por su cuenta a determinados sitios como por ejemplo un hospital o centro de salud, un centro cultural o deportivo, etc"="M1P1_0_7",
                                                                                       "3.8. (Económica) Se niega o se ha negado a darle dinero para los gastos del hogar cuando la pareja tiene/tenía dinero para otras cosas."="M1P1_0_8",
                                                                                       "3.9. (Económica) Le impide o ha impedido tomar decisiones relacionadas con la economía familiar y/o realizar las compras de forma independiente"="M1P1_0_9",
                                                                                       "3.10. (Económica) No le deja o no le ha dejado trabajar o estudiar fuera del hogar"="M1P1_0_10",
                                                                                       "3.11. (Económica) Usa o ha usado su dinero o su tarjeta de crédito o pide préstamos a su nombre sin su consentimiento"="M1P1_0_11",
                                                                                       "3.12. (Emocional) Le ha insultado o hecho sentirse mal con usted misma"="M1P3_0_1",
                                                                                       "3.13. (Emocional) Le ha menospreciado o humillado delante de otras personas"="M1P3_0_2",
                                                                                       "3.14. (Emocional) Le ha asustado o intimidado a propósito (por ejemplo gritándole, rompiendo cosas, golpeando paredes o mirándola de determinada forma)"="M1P3_0_3",
                                                                                       "3.15. (Emocional) Le ha amenazado verbalmente con hacerle daño a Ud"="M1P3_0_4",
                                                                                       "3.16. (Emocional) Le ha amenazado verbalmente con hacerle daño a sus hijos/as o a alguna otra persona que es/era importante para Ud."="M1P3_0_5",
                                                                                       "3.17. (Emocional) Le ha amenazado con hacerse daño a sí mismo/a si usted le/la deja."="M1P3_0_6",
                                                                                       "3.18. (Emocional) Le ha amenazado con quitarle a sus hijos/as"="M1P3_0_7",
                                                                                       "3.19. (Miedo) ¿Ha tenido o tiene Ud. miedo de su pareja actual?"="M1P6")),
                                 actionLink("selectallPsico","Seleccionar todo"))),
                        tabPanel("Violencia de Exparejas",
                                 wellPanel(style = "background: #d9d4e6",
                                           checkboxGroupInput("VioPEx","Violencia Psicológica en Exparejas",
                                                    choices=list("4.1. (De Control) Trataba de impedirle que viera a sus amigos o amigas"="M2P1_0_1",
                                                                 "4.2. (De Control) Trataba de evitar que Ud. se relacionase con su familia directa o parientes"="M2P1_0_2",
                                                                 "4.3. (De Control) Insistía en saber dónde estaba usted en cada momento"="M2P1_0_3",
                                                                 "4.4. (De Control) Le ignoraba o le trataba con indiferencia"="M2P1_0_4",
                                                                 "4.5. (De Control) Se enfadaba si hablaba con otro hombre o mujer"="M2P1_0_5",
                                                                 "4.6. (De Control) Sospechaba sin motivos que Ud. le era infiel"="M2P1_0_6",
                                                                 "4.7. (De Control) Esperaba que Ud. le pidiera permiso antes de ir por su cuenta a determinados sitios como por ejemplo un hospital o centro de salud, un centro cultural o deportivo, etc"="M2P1_0_7",
                                                                 "4.8. (De Control) Se negaba a darle dinero para los gastos del hogar cuando la pareja tenía dinero para otras cosas"="M2P1_0_8",
                                                                 "4.9. (De Control) Le impedía tomar decisiones relacionadas con la economía familiar y/o realizar las compras de forma independiente"="M2P1_0_9",
                                                                 "4.10. (De Control) No le dejaba trabajar o estudiar fuera del hogar"="M2P1_0_10",
                                                                 "4.11. (De Control) Usaba su dinero o su tarjeta de crédito o pedía préstamos a su nombre sin su consentimiento"="M2P1_0_11",
                                                                 "4.12. (Emocional) Le insultó o hizo sentirse mal con usted misma"="M2P3_0_1",
                                                                 "4.13. (Emocional) Le menospreció o humilló delante de otras personas"="M2P3_0_2",
                                                                 "4.14. (Emocional) Le asustó o intimidó a propósito (por ejemplo gritándole y rompiendo cosas, mirándole de determinada forma)"="M2P3_0_3",
                                                                 "4.15. (Emocional) Le amenazó verbalmente con hacerle daño a Ud."="M2P3_0_4",
                                                                 "4.16. (Emocional) Le amenazó verbalmente con hacerle daño a sus hijos/as o a alguna otra persona que es/era importante para Ud."="M2P3_0_5",
                                                                 "4.17. (Emocional) Le amenazó con hacerse daño a si mismo/a si usted le/la dejaba."="M2P3_0_6",
                                                                 "4.18. (Emocional) Le amenazó con quitarle a sus hijos/as"="M2P3_0_7",
                                                                 "4.19. (Miedo) ¿Ha tenido o tiene Ud. miedo de alguna de su/s pareja/s del pasado?"="M2P6")),
                                 actionLink("selectallPsicoEx","Seleccionar todo")))),
             tabPanel("Violencia Fuera Pareja",
                      wellPanel(style = "background: #d9d4e6",
                                checkboxGroupInput("VioFP","Violencia Fuera de la Pareja (cualquier otra persona que no sea una pareja)",
                                         choices=list("5.1. (Física) Le ha abofeteado o tirado algo que pudiese hacerle daño"="M3P1_1",
                                                      "5.2. (Física) Le ha empujado, agarrado o tirado del pelo"="M3P1_2",
                                                      "5.3. (Física) Le ha golpeado con su puño o con alguna otra cosa que pudiese hacerle daño"="M3P1_3",
                                                      "5.4. (Física) Le ha dado patadas, arrastrado o pegado una paliza"="M3P1_4",
                                                      "5.5. (Física) Le ha intentado asfixiar o quemar a propósito"="M3P1_5",
                                                      "5.6. (Física) Le ha amenazado con usar una pistola, cuchillo u otra arma o substancia peligrosa contra Ud"="M3P1_6",
                                                      "5.7. (Física) Ha usado la fuerza contra usted, de cualquier otra manera de las mencionadas anteriormente, de forma que le ha hecho daño o podría haberle hecho daño"="M3P1_7",
                                                      "5.8. (Sexual) Le ha obligado a mantener relaciones sexuales amenazándole, sujetándole o haciéndole daño de alguna manera. Por relaciones sexuales queremos decir penetración vaginal o anal con pene u objetos, o sexo oral"="M3P2_1",
                                                      "5.9. (Sexual) Le ha obligado a mantener relaciones sexuales cuando era incapaz de rechazarlas debido a que usted estaba bajo la influencia del alcohol o las drogas"="M3P2_2",
                                                      "5.10. (Sexual) Ha mantenido relaciones sexuales sin desearlo porque tenía miedo de lo que esa persona le podría hacer si se negaba"="M3P2_3",
                                                      "5.11. (Sexual) Le ha obligado a mantener relaciones sexuales cuando Ud. no quería"="M3P2_4",
                                                      "5.12. (Sexual) Ha intentado obligarle a tener relaciones sexuales contra su voluntad sin conseguirlo"="M3P2_5",
                                                      "5.13. (Sexual) Le ha tocado a Ud. sus partes íntimas – genitales o pecho- o le ha realizado algún otro tocamiento de tipo sexual cuando usted no quería"="M3P2_6",
                                                      "5.14. (Sexual) Le ha hecho alguna vez tocarle sus partes íntimas – genitales o pecho- o le ha obligado a realizarle algún otro tocamiento de tipo sexual cuando usted no quería"="M3P2_7",
                                                      "5.15. (Sexual) Le ha obligado a realizar alguna otra práctica de tipo sexual que yo no le haya mencionado ya"="M3P2_8")),
                      actionLink("selectallFP","Seleccionar todo"))),
             tabPanel("Acoso Sexual",
                      wellPanel(style = "background: #d9d4e6",
                                checkboxGroupInput("VioAS","Acoso Sexual",
                                                        choices=list("6.1. ¿En alguna ocasión ha sufrido miradas insistentes o lascivas que le hayan hecho sentirse intimidada"="M4P1_1",
                                                                     "6.2. Alguien le mostró o envió imágenes o fotos sexualmente explícitas que le hayan hecho sentirse ofendida, humillada, o intimidada"="M4P1_2",
                                                                     "6.3. Ha recibido bromas sexuales o comentarios ofensivos sobre su cuerpo o su vida privada"="M4P1_3",
                                                                     "6.4. Ha tenido sugerencias inapropiadas para tener una cita o para cualquier actividad de tipo sexual, que le hayan hecho sentir ofendida, humillada, o intimidada"="M4P1_4",
                                                                     "6.5. Ha tenido contacto físico no deseado como, por ejemplo, proximidad innecesariamente cercana, tocamientos de partes de su cuerpo, besos/abrazos, o cualquier otra cosa que usted no quisiera"="M4P1_5",
                                                                     "6.6. Ha recibido insinuaciones inapropiadas, humillantes, intimidatorias, u ofensivas en las redes sociales de internet como Facebook, Instagram o Twitter"="M4P1_6",
                                                                     "6.7. Ha recibido correos electrónicos, mensajes de Whatsapp, o mensajes de texto sexualmente explícitos inapropiados, que le hayan hecho sentir ofendida, humillada, o intimidada"="M4P1_7",
                                                                     "6.8. Que alguien le haya amenazado con consecuencias desagradables en su trabajo, como por ejemplo un despido, si rechazaba las propuestas o avances sexuales?"="M4P1_8",
                                                                     "6.9. Alguien que se le haya exhibido indecentemente"="M4P1_9",
                                                                     "6.10. Alguien que le haya obligado a ver material pornográfico contra su voluntad"="M4P1_10",
                                                                     "6.11. Otros comportamientos similares con una connotación sexual que le hayan hecho sentirse ofendida, humillada o intimidada, y que no hayan sido mencionados previamente?"="M4P1_11")),
                      actionLink("selectallAS","Seleccionar todo"))),
             tabPanel("Stalking",
                      wellPanel(style = "background: #d9d4e6",
                                checkboxGroupInput("VioAR","Acoso Reiterado o Stalking",
                                                   choices=list("7.1. Le ha enviado mensajes no deseados, llamadas telefónicas, emails, cartas o regalos"="M5P1_1",
                                                                "7.2. Le ha hecho llamadas telefónicas obscenas, amenazantes, molestas o silenciosas"="M5P1_2",
                                                                "7.3. Le ha esperado o ha estado merodeando fuera de su casa, colegio o trabajo"="M5P1_3",
                                                                "7.4. Le ha seguido o espiado"="M5P1_4",
                                                                "7.5. Ha dañado intencionadamente cosas suyas (coche, moto, buzón, etc.) o las propiedades de personas que le importan, o ha dañado a sus animales"="M5P1_5",
                                                                "7.6. Ha hecho comentarios ofensivos o embarazosos sobre usted, le ha hecho propuestas inapropiadas en internet o en redes sociales"="M5P1_6",
                                                                "7.7. Ha publicado fotos, vídeos o información muy personal sobre Ud. en lugares como su vecindario, trabajo, escuela, internet, o redes sociales como Facebook o Instagram, o ha enviado esta información a otras personas a través de teléfonos móviles o aplicaciones como Whatsapp"="M5P1_7")),
                      actionLink("selectallAR","Seleccionar todo")),
                      tags$style(HTML(".navbar-default .navbar-nav > li > a[data-value='Página Principal'] {color: #000000;background-color: #d9d4e6;}
.navbar-default .navbar-nav > li > a[data-value='Violencia Física y Sexual'] {color: #432b82;background-color: #d9d4e6;}
                             .navbar-default .navbar-nav > li > a[data-value='Violencia Psicológica'] {color: #432b82;background-color: #d9d4e6;}
                             .navbar-default .navbar-nav > li > a[data-value='Violencia Fuera Pareja'] {color: #432b82;background-color: #d9d4e6;}
                             .navbar-default .navbar-nav > li > a[data-value='Acoso Sexual'] {color: #432b82;background-color: #d9d4e6;}
                             .navbar-default .navbar-nav > li > a[data-value='Stalking'] {color: #432b82;background-color: #d9d4e6;}
                             #selectallAR{color:#432b82;background-color: #c6bfd9;}
                                      #selectallAS{color:#432b82;background-color: #c6bfd9;}
                                      #selectallFP{color:#432b82;background-color: #c6bfd9;}
                                      #selectallPsicoEx{color:#432b82;background-color:#c6bfd9;}
                                      #selectallPsico{color:#432b82;background-color: #c6bfd9;}
                                      #selectallFySEx{color:#432b82;background-color: #c6bfd9;}
                                      #selectallFyS{color:#432b82;background-color: #c6bfd9;}
                                      .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #c6bfd9}
                                      .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #c6bfd9}
                                      .js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #c6bfd9}"))))
)

server = function(input, output, session) {
  
  observe({
    if(input$selectallFyS == 0) return(NULL) 
    else if (input$selectallFyS%%2 == 0)
    {
      updateCheckboxGroupInput(session,"VioFS","Violencia Fisica y Sexual",choices=list("1.1. (Física) Le ha abofeteado o tirado algo que pudiese hacerle daño"="M1P4_0_1",
                                                                                        "1.2. (Física) Le ha empujado, agarrado o tirado del pelo"="M1P4_0_2",
                                                                                        "1.3. (Física) Le ha golpeado con su puño o con alguna otra cosa que pudiese hacerle daño"="M1P4_0_3",
                                                                                        "1.4. (Física) Le ha dado patadas, arrastrado o pegado una paliza"="M1P4_0_4",
                                                                                        "1.5. (Física) Le ha intentado asfixiar o quemar a propósito"="M1P4_0_5",
                                                                                        "1.6. (Física) Le ha amenazado con usar o ha usado una pistola, cuchillo u otra arma o substancia peligrosa contra Ud."="M1P4_0_6",
                                                                                        "1.7. (Física) Ha usado la fuerza contra usted, de cualquier otra manera de las mencionadas anteriormente, de forma que le ha hecho daño o podría haberle hecho daño"="M1P4_0_7",
                                                                                        "1.8.(Sexual) Le ha obligado a mantener relaciones sexuales amenazándole, sujetándole o haciéndole daño de alguna manera. Por relaciones sexuales queremos decir penetración vaginal o anal con pene u objetos, o sexo oral"="M1P5_0_1",
                                                                                        "1.9. (Sexual) Le ha hecho mantener relaciones sexuales cuando era incapaz de rechazarlas debido a que usted estaba bajo la influencia del alcohol o las drogas"="M1P5_0_2",
                                                                                        "1.10. (Sexual) Ha mantenido relaciones sexuales sin desearlo porque tenía miedo de lo que le podría hacer si se negaba"="M1P5_0_3",
                                                                                        "1.11. (Sexual) Le ha obligado a mantener relaciones sexuales cuando Ud. no quería"="M1P5_0_4",
                                                                                        "1.12. (Sexual) Ha intentado obligarle a tener relaciones sexuales contra su voluntad sin conseguirlo"="M1P5_0_5",
                                                                                        "1.13. (Sexual) Le ha tocado a Ud. sus partes íntimas – genitales o pecho- o le ha realizado algún otro tipo de tocamiento de tipo sexual cuando usted no quería"="M1P5_0_6",
                                                                                        "1.14. (Sexual) Le ha hecho alguna vez tocarle sus partes íntimas – genitales o pecho- o le ha obligado a realizarle algún otro tipo de tocamiento de tipo sexual cuando usted no quería"="M1P5_0_7",
                                                                                        "1.15. (Sexual) Le ha obligado a realizar alguna otra práctica de tipo sexual que yo no le haya mencionado ya"="M1P5_0_8"))
    }
    else
    {
      updateCheckboxGroupInput(session,"VioFS","Violencia Fisica y Sexual",choices=list("1.1. (Física) Le ha abofeteado o tirado algo que pudiese hacerle daño"="M1P4_0_1",
                                                                                        "1.2. (Física) Le ha empujado, agarrado o tirado del pelo"="M1P4_0_2",
                                                                                        "1.3. (Física) Le ha golpeado con su puño o con alguna otra cosa que pudiese hacerle daño"="M1P4_0_3",
                                                                                        "1.4. (Física) Le ha dado patadas, arrastrado o pegado una paliza"="M1P4_0_4",
                                                                                        "1.5. (Física) Le ha intentado asfixiar o quemar a propósito"="M1P4_0_5",
                                                                                        "1.6. (Física) Le ha amenazado con usar o ha usado una pistola, cuchillo u otra arma o substancia peligrosa contra Ud."="M1P4_0_6",
                                                                                        "1.7. (Física) Ha usado la fuerza contra usted, de cualquier otra manera de las mencionadas anteriormente, de forma que le ha hecho daño o podría haberle hecho daño"="M1P4_0_7",
                                                                                        "1.8.(Sexual) Le ha obligado a mantener relaciones sexuales amenazándole, sujetándole o haciéndole daño de alguna manera. Por relaciones sexuales queremos decir penetración vaginal o anal con pene u objetos, o sexo oral"="M1P5_0_1",
                                                                                        "1.9. (Sexual) Le ha hecho mantener relaciones sexuales cuando era incapaz de rechazarlas debido a que usted estaba bajo la influencia del alcohol o las drogas"="M1P5_0_2",
                                                                                        "1.10. (Sexual) Ha mantenido relaciones sexuales sin desearlo porque tenía miedo de lo que le podría hacer si se negaba"="M1P5_0_3",
                                                                                        "1.11. (Sexual) Le ha obligado a mantener relaciones sexuales cuando Ud. no quería"="M1P5_0_4",
                                                                                        "1.12. (Sexual) Ha intentado obligarle a tener relaciones sexuales contra su voluntad sin conseguirlo"="M1P5_0_5",
                                                                                        "1.13. (Sexual) Le ha tocado a Ud. sus partes íntimas – genitales o pecho- o le ha realizado algún otro tipo de tocamiento de tipo sexual cuando usted no quería"="M1P5_0_6",
                                                                                        "1.14. (Sexual) Le ha hecho alguna vez tocarle sus partes íntimas – genitales o pecho- o le ha obligado a realizarle algún otro tipo de tocamiento de tipo sexual cuando usted no quería"="M1P5_0_7",
                                                                                        "1.15. (Sexual) Le ha obligado a realizar alguna otra práctica de tipo sexual que yo no le haya mencionado ya"="M1P5_0_8"),selected=c("M1P4_0_1","M1P4_0_2","M1P4_0_3","M1P4_0_4","M1P4_0_5","M1P4_0_6","M1P4_0_7","M1P5_0_1","M1P5_0_2","M1P5_0_3","M1P5_0_4","M1P5_0_5","M1P5_0_6","M1P5_0_7","M1P5_0_8"))
    }
    
  })
  
  observe({
    if(input$selectallFySEx == 0) return(NULL) 
    else if (input$selectallFySEx%%2 == 0)
    {
      updateCheckboxGroupInput(session,"VioFSEx","Violencia Fisica y Sexual",choices=list("2.1. (Física) Le abofeteó o tiró algo que pudiese hacerle daño"="M2P4_0_1",
                                                                                          "2.2. (Física) Le empujó, agarró o tiró del pelo"="M2P4_0_2",
                                                                                          "2.3. (Física) Le golpeó con su puño o con alguna otra cosa que pudiese hacerle daño"="M2P4_0_3",
                                                                                          "2.4. (Física) Le dio patadas, arrastró o pegó una paliza"="M2P4_0_4",
                                                                                          "2.5. (Física) Le intentó asfixiar o quemar a propósito"="M2P4_0_5",
                                                                                          "2.6. (Física) Le amenazó con usar o usó una pistola, cuchillo u otra arma o substancia peligrosa contra Ud."="M2P4_0_6",
                                                                                          "2.7. (Física) Usó la fuerza contra usted, de cualquier otra manera de las mencionadas anteriormente, de forma que le hizo daño o podría haberle hecho daño"="M2P4_0_7",
                                                                                          "2.8. (Sexual) Le obligó a mantener relaciones sexuales amenazándole, sujetándole o haciéndole daño de alguna manera. Por relaciones sexuales queremos decir penetración vaginal o anal con pene u objetos, o sexo oral"="M2P5_0_1",
                                                                                          "2.9. (Sexual) Le hizo mantener relaciones sexuales cuando era incapaz de rechazarlas debido a que usted estaba bajo la influencia del alcohol o las drogas"="M2P5_0_2",
                                                                                          "2.10. (Sexual) Mantuvo relaciones sexuales sin desearlo porque tenía miedo de lo que le podría hacer si se negaba"="M2P5_0_3",
                                                                                          "2.11. (Sexual) Le obligó a mantener relaciones sexuales cuando Ud. no quería"="M2P5_0_4",
                                                                                          "2.12. (Sexual) Intentó obligarle a tener relaciones sexuales contra su voluntad sin conseguirlo"="M2P5_0_5",
                                                                                          "2.13. (Sexual) Le tocó a Ud. sus partes íntimas – genitales o pecho o le realizó algún otro tipo de tocamiento de tipo sexual cuando usted no quería"="M2P5_0_6",
                                                                                          "2.14. (Sexual) Le hizo alguna vez tocarle sus partes íntimas – genitales o pecho- o le obligó a realizarle algún otro tipo de tocamiento de tipo sexual cuando usted no quería"="M2P5_0_7",
                                                                                          "2.15. (Sexual) Le obligó a realizar alguna otra práctica de tipo sexual que yo no le haya mencionado ya"="M2P5_0_8"))
    }
    else
    {
      updateCheckboxGroupInput(session,"VioFSEx","Violencia Fisica y Sexual",choices=list("2.1. (Física) Le abofeteó o tiró algo que pudiese hacerle daño"="M2P4_0_1",
                                                                                          "2.2. (Física) Le empujó, agarró o tiró del pelo"="M2P4_0_2",
                                                                                          "2.3. (Física) Le golpeó con su puño o con alguna otra cosa que pudiese hacerle daño"="M2P4_0_3",
                                                                                          "2.4. (Física) Le dio patadas, arrastró o pegó una paliza"="M2P4_0_4",
                                                                                          "2.5. (Física) Le intentó asfixiar o quemar a propósito"="M2P4_0_5",
                                                                                          "2.6. (Física) Le amenazó con usar o usó una pistola, cuchillo u otra arma o substancia peligrosa contra Ud."="M2P4_0_6",
                                                                                          "2.7. (Física) Usó la fuerza contra usted, de cualquier otra manera de las mencionadas anteriormente, de forma que le hizo daño o podría haberle hecho daño"="M2P4_0_7",
                                                                                          "2.8. (Sexual) Le obligó a mantener relaciones sexuales amenazándole, sujetándole o haciéndole daño de alguna manera. Por relaciones sexuales queremos decir penetración vaginal o anal con pene u objetos, o sexo oral"="M2P5_0_1",
                                                                                          "2.9. (Sexual) Le hizo mantener relaciones sexuales cuando era incapaz de rechazarlas debido a que usted estaba bajo la influencia del alcohol o las drogas"="M2P5_0_2",
                                                                                          "2.10. (Sexual) Mantuvo relaciones sexuales sin desearlo porque tenía miedo de lo que le podría hacer si se negaba"="M2P5_0_3",
                                                                                          "2.11. (Sexual) Le obligó a mantener relaciones sexuales cuando Ud. no quería"="M2P5_0_4",
                                                                                          "2.12. (Sexual) Intentó obligarle a tener relaciones sexuales contra su voluntad sin conseguirlo"="M2P5_0_5",
                                                                                          "2.13. (Sexual) Le tocó a Ud. sus partes íntimas – genitales o pecho o le realizó algún otro tipo de tocamiento de tipo sexual cuando usted no quería"="M2P5_0_6",
                                                                                          "2.14. (Sexual) Le hizo alguna vez tocarle sus partes íntimas – genitales o pecho- o le obligó a realizarle algún otro tipo de tocamiento de tipo sexual cuando usted no quería"="M2P5_0_7",
                                                                                          "2.15. (Sexual) Le obligó a realizar alguna otra práctica de tipo sexual que yo no le haya mencionado ya"="M2P5_0_8"),selected=c("M2P4_0_1","M2P4_0_2","M2P4_0_3","M2P4_0_4","M2P4_0_5","M2P4_0_6","M2P4_0_7","M2P5_0_1","M2P5_0_2","M2P5_0_3","M2P5_0_4","M2P5_0_5","M2P5_0_6","M2P5_0_7","M2P5_0_8"))
    }
    
  })
  
  observe({
    if(input$selectallPsico == 0) return(NULL) 
    else if (input$selectallPsico%%2 == 0)
    {
      updateCheckboxGroupInput(session,"VioP","En parejas",
                               choices=list("3.1. (De Control) Trata o ha tratado de impedirle que vea a sus amigos o amigas"="M1P1_0_1",
                                            "3.2. (De Control) Trata o ha tratado de evitar que Ud. se relacione con su familia directa o parientes"="M1P1_0_2",
                                            "3.3. (De Control) Insiste o ha insistido en saber dónde está usted en cada momento."="M1P1_0_3",
                                            "3.4. (De Control) Le ignora o ha ignorado y le trata o ha tratado con indiferencia"="M1P1_0_4",
                                            "3.5. (De Control) Se enfada o se ha enfadado si habla con otro hombre o mujer"="M1P1_0_5",
                                            "3.6. (De Control) Sospecha o ha sospechado sin motivos que Ud. le es/era infiel"="M1P1_0_6",
                                            "3.7. (De Control) Espera o ha esperado que Ud. le pida permiso antes de ir por su cuenta a determinados sitios como por ejemplo un hospital o centro de salud, un centro cultural o deportivo, etc"="M1P1_0_7",
                                            "3.8. (Económica) Se niega o se ha negado a darle dinero para los gastos del hogar cuando la pareja tiene/tenía dinero para otras cosas."="M1P1_0_8",
                                            "3.9. (Económica) Le impide o ha impedido tomar decisiones relacionadas con la economía familiar y/o realizar las compras de forma independiente"="M1P1_0_9",
                                            "3.10. (Económica) No le deja o no le ha dejado trabajar o estudiar fuera del hogar"="M1P1_0_10",
                                            "3.11. (Económica) Usa o ha usado su dinero o su tarjeta de crédito o pide préstamos a su nombre sin su consentimiento"="M1P1_0_11",
                                            "3.12. (Emocional) Le ha insultado o hecho sentirse mal con usted misma"="M1P3_0_1",
                                            "3.13. (Emocional) Le ha menospreciado o humillado delante de otras personas"="M1P3_0_2",
                                            "3.14. (Emocional) Le ha asustado o intimidado a propósito (por ejemplo gritándole, rompiendo cosas, golpeando paredes o mirándola de determinada forma)"="M1P3_0_3",
                                            "3.15. (Emocional) Le ha amenazado verbalmente con hacerle daño a Ud"="M1P3_0_4",
                                            "3.16. (Emocional) Le ha amenazado verbalmente con hacerle daño a sus hijos/as o a alguna otra persona que es/era importante para Ud."="M1P3_0_5",
                                            "3.17. (Emocional) Le ha amenazado con hacerse daño a sí mismo/a si usted le/la deja."="M1P3_0_6",
                                            "3.18. (Emocional) Le ha amenazado con quitarle a sus hijos/as"="M1P3_0_7",
                                            "3.19. (Miedo) ¿Ha tenido o tiene Ud. miedo de su pareja actual?"="M1P6"))
    }
    else
    {
      updateCheckboxGroupInput(session,"VioP","En parejas",choices=list("3.1. (De Control) Trata o ha tratado de impedirle que vea a sus amigos o amigas"="M1P1_0_1",
                                                                                        "3.2. (De Control) Trata o ha tratado de evitar que Ud. se relacione con su familia directa o parientes"="M1P1_0_2",
                                                                                        "3.3. (De Control) Insiste o ha insistido en saber dónde está usted en cada momento."="M1P1_0_3",
                                                                                        "3.4. (De Control) Le ignora o ha ignorado y le trata o ha tratado con indiferencia"="M1P1_0_4",
                                                                                        "3.5. (De Control) Se enfada o se ha enfadado si habla con otro hombre o mujer"="M1P1_0_5",
                                                                                        "3.6. (De Control) Sospecha o ha sospechado sin motivos que Ud. le es/era infiel"="M1P1_0_6",
                                                                                        "3.7. (De Control) Espera o ha esperado que Ud. le pida permiso antes de ir por su cuenta a determinados sitios como por ejemplo un hospital o centro de salud, un centro cultural o deportivo, etc"="M1P1_0_7",
                                                                                        "3.8. (Económica) Se niega o se ha negado a darle dinero para los gastos del hogar cuando la pareja tiene/tenía dinero para otras cosas."="M1P1_0_8",
                                                                                        "3.9. (Económica) Le impide o ha impedido tomar decisiones relacionadas con la economía familiar y/o realizar las compras de forma independiente"="M1P1_0_9",
                                                                                        "3.10. (Económica) No le deja o no le ha dejado trabajar o estudiar fuera del hogar"="M1P1_0_10",
                                                                                        "3.11. (Económica) Usa o ha usado su dinero o su tarjeta de crédito o pide préstamos a su nombre sin su consentimiento"="M1P1_0_11",
                                                                                        "3.12. (Emocional) Le ha insultado o hecho sentirse mal con usted misma"="M1P3_0_1",
                                                                                        "3.13. (Emocional) Le ha menospreciado o humillado delante de otras personas"="M1P3_0_2",
                                                                                        "3.14. (Emocional) Le ha asustado o intimidado a propósito (por ejemplo gritándole, rompiendo cosas, golpeando paredes o mirándola de determinada forma)"="M1P3_0_3",
                                                                                        "3.15. (Emocional) Le ha amenazado verbalmente con hacerle daño a Ud"="M1P3_0_4",
                                                                                        "3.16. (Emocional) Le ha amenazado verbalmente con hacerle daño a sus hijos/as o a alguna otra persona que es/era importante para Ud."="M1P3_0_5",
                                                                                        "3.17. (Emocional) Le ha amenazado con hacerse daño a sí mismo/a si usted le/la deja."="M1P3_0_6",
                                                                                        "3.18. (Emocional) Le ha amenazado con quitarle a sus hijos/as"="M1P3_0_7",
                                                                                        "3.19. (Miedo) ¿Ha tenido o tiene Ud. miedo de su pareja actual?"="M1P6"), selected=c("M1P1_0_1","M1P1_0_2","M1P1_0_3","M1P1_0_4","M1P1_0_5",
                                                                                                                                                                              "M1P1_0_6","M1P1_0_7","M1P1_0_8","M1P1_0_9","M1P1_0_10",
                                                                                                                                                                              "M1P1_0_11","M1P3_0_1","M1P3_0_2","M1P3_0_3","M1P3_0_4",
                                                                                                                                                                              "M1P3_0_5","M1P3_0_6","M1P3_0_7","M1P6"))
    }
    
  })
  
  observe({
    if(input$selectallPsicoEx == 0) return(NULL) 
    else if (input$selectallPsicoEx%%2 == 0)
    {
      updateCheckboxGroupInput(session,"VioPEx","En Exparejas",
                               choices=list("4.1. (De Control) Trataba de impedirle que viera a sus amigos o amigas"="M2P1_0_1",
                                            "4.2. (De Control) Trataba de evitar que Ud. se relacionase con su familia directa o parientes"="M2P1_0_2",
                                            "4.3. (De Control) Insistía en saber dónde estaba usted en cada momento"="M2P1_0_3",
                                            "4.4. (De Control) Le ignoraba o le trataba con indiferencia"="M2P1_0_4",
                                            "4.5. (De Control) Se enfadaba si hablaba con otro hombre o mujer"="M2P1_0_5",
                                            "4.6. (De Control) Sospechaba sin motivos que Ud. le era infiel"="M2P1_0_6",
                                            "4.7. (De Control) Esperaba que Ud. le pidiera permiso antes de ir por su cuenta a determinados sitios como por ejemplo un hospital o centro de salud, un centro cultural o deportivo, etc"="M2P1_0_7",
                                            "4.8. (De Control) Se negaba a darle dinero para los gastos del hogar cuando la pareja tenía dinero para otras cosas"="M2P1_0_8",
                                            "4.9. (De Control) Le impedía tomar decisiones relacionadas con la economía familiar y/o realizar las compras de forma independiente"="M2P1_0_9",
                                            "4.10. (De Control) No le dejaba trabajar o estudiar fuera del hogar"="M2P1_0_10",
                                            "4.11. (De Control) Usaba su dinero o su tarjeta de crédito o pedía préstamos a su nombre sin su consentimiento"="M2P1_0_11",
                                            "4.12. (Emocional) Le insultó o hizo sentirse mal con usted misma"="M2P3_0_1",
                                            "4.13. (Emocional) Le menospreció o humilló delante de otras personas"="M2P3_0_2",
                                            "4.14. (Emocional) Le asustó o intimidó a propósito (por ejemplo gritándole y rompiendo cosas, mirándole de determinada forma)"="M2P3_0_3",
                                            "4.15. (Emocional) Le amenazó verbalmente con hacerle daño a Ud."="M2P3_0_4",
                                            "4.16. (Emocional) Le amenazó verbalmente con hacerle daño a sus hijos/as o a alguna otra persona que es/era importante para Ud."="M2P3_0_5",
                                            "4.17. (Emocional) Le amenazó con hacerse daño a si mismo/a si usted le/la dejaba."="M2P3_0_6",
                                            "4.18. (Emocional) Le amenazó con quitarle a sus hijos/as"="M2P3_0_7",
                                            "4.19. (Miedo) ¿Ha tenido o tiene Ud. miedo de alguna de su/s pareja/s del pasado?"="M2P6"))
    }
    else
    {
      updateCheckboxGroupInput(session,"VioPEx","En Exparejas",
                               choices=list("4.1. (De Control) Trataba de impedirle que viera a sus amigos o amigas"="M2P1_0_1",
                                            "4.2. (De Control) Trataba de evitar que Ud. se relacionase con su familia directa o parientes"="M2P1_0_2",
                                            "4.3. (De Control) Insistía en saber dónde estaba usted en cada momento"="M2P1_0_3",
                                            "4.4. (De Control) Le ignoraba o le trataba con indiferencia"="M2P1_0_4",
                                            "4.5. (De Control) Se enfadaba si hablaba con otro hombre o mujer"="M2P1_0_5",
                                            "4.6. (De Control) Sospechaba sin motivos que Ud. le era infiel"="M2P1_0_6",
                                            "4.7. (De Control) Esperaba que Ud. le pidiera permiso antes de ir por su cuenta a determinados sitios como por ejemplo un hospital o centro de salud, un centro cultural o deportivo, etc"="M2P1_0_7",
                                            "4.8. (De Control) Se negaba a darle dinero para los gastos del hogar cuando la pareja tenía dinero para otras cosas"="M2P1_0_8",
                                            "4.9. (De Control) Le impedía tomar decisiones relacionadas con la economía familiar y/o realizar las compras de forma independiente"="M2P1_0_9",
                                            "4.10. (De Control) No le dejaba trabajar o estudiar fuera del hogar"="M2P1_0_10",
                                            "4.11. (De Control) Usaba su dinero o su tarjeta de crédito o pedía préstamos a su nombre sin su consentimiento"="M2P1_0_11",
                                            "4.12. (Emocional) Le insultó o hizo sentirse mal con usted misma"="M2P3_0_1",
                                            "4.13. (Emocional) Le menospreció o humilló delante de otras personas"="M2P3_0_2",
                                            "4.14. (Emocional) Le asustó o intimidó a propósito (por ejemplo gritándole y rompiendo cosas, mirándole de determinada forma)"="M2P3_0_3",
                                            "4.15. (Emocional) Le amenazó verbalmente con hacerle daño a Ud."="M2P3_0_4",
                                            "4.16. (Emocional) Le amenazó verbalmente con hacerle daño a sus hijos/as o a alguna otra persona que es/era importante para Ud."="M2P3_0_5",
                                            "4.17. (Emocional) Le amenazó con hacerse daño a si mismo/a si usted le/la dejaba."="M2P3_0_6",
                                            "4.18. (Emocional) Le amenazó con quitarle a sus hijos/as"="M2P3_0_7",
                                            "4.19. (Miedo) ¿Ha tenido o tiene Ud. miedo de alguna de su/s pareja/s del pasado?"="M2P6"), selected=c("M2P1_0_1","M2P1_0_2","M2P1_0_3","M2P1_0_4","M2P1_0_5",
                                                                                                                                                        "M2P1_0_6","M2P1_0_7","M2P1_0_8","M2P1_0_9","M2P1_0_10",
                                                                                                                                                        "M2P1_0_11","M2P3_0_1","M2P3_0_2","M2P3_0_3","M2P3_0_4",
                                                                                                                                                        "M2P3_0_5","M2P3_0_6","M2P3_0_7","M2P6" ))
    }
    
  })
  
  observe({
    if(input$selectallFP == 0) return(NULL) 
    else if (input$selectallFP%%2 == 0)
    {
      updateCheckboxGroupInput(session,"VioFP","Violencia Fuera de la Pareja",
                               choices=list("5.1. (Física) Le ha abofeteado o tirado algo que pudiese hacerle daño"="M3P1_1",
                                            "5.2. (Física) Le ha empujado, agarrado o tirado del pelo"="M3P1_2",
                                            "5.3. (Física) Le ha golpeado con su puño o con alguna otra cosa que pudiese hacerle daño"="M3P1_3",
                                            "5.4. (Física) Le ha dado patadas, arrastrado o pegado una paliza"="M3P1_4",
                                            "5.5. (Física) Le ha intentado asfixiar o quemar a propósito"="M3P1_5",
                                            "5.6. (Física) Le ha amenazado con usar una pistola, cuchillo u otra arma o substancia peligrosa contra Ud"="M3P1_6",
                                            "5.7. (Física) Ha usado la fuerza contra usted, de cualquier otra manera de las mencionadas anteriormente, de forma que le ha hecho daño o podría haberle hecho daño"="M3P1_7",
                                            "5.8. (Sexual) Le ha obligado a mantener relaciones sexuales amenazándole, sujetándole o haciéndole daño de alguna manera. Por relaciones sexuales queremos decir penetración vaginal o anal con pene u objetos, o sexo oral"="M3P2_1",
                                            "5.9. (Sexual) Le ha obligado a mantener relaciones sexuales cuando era incapaz de rechazarlas debido a que usted estaba bajo la influencia del alcohol o las drogas"="M3P2_2",
                                            "5.10. (Sexual) Ha mantenido relaciones sexuales sin desearlo porque tenía miedo de lo que esa persona le podría hacer si se negaba"="M3P2_3",
                                            "5.11. (Sexual) Le ha obligado a mantener relaciones sexuales cuando Ud. no quería"="M3P2_4",
                                            "5.12. (Sexual) Ha intentado obligarle a tener relaciones sexuales contra su voluntad sin conseguirlo"="M3P2_5",
                                            "5.13. (Sexual) Le ha tocado a Ud. sus partes íntimas – genitales o pecho- o le ha realizado algún otro tocamiento de tipo sexual cuando usted no quería"="M3P2_6",
                                            "5.14. (Sexual) Le ha hecho alguna vez tocarle sus partes íntimas – genitales o pecho- o le ha obligado a realizarle algún otro tocamiento de tipo sexual cuando usted no quería"="M3P2_7",
                                            "5.15. (Sexual) Le ha obligado a realizar alguna otra práctica de tipo sexual que yo no le haya mencionado ya"="M3P2_8"))
    }
    else
    {
      updateCheckboxGroupInput(session,"VioFP","Violencia Fuera de la Pareja",
                               choices=list("5.1. (Física) Le ha abofeteado o tirado algo que pudiese hacerle daño"="M3P1_1",
                                            "5.2. (Física) Le ha empujado, agarrado o tirado del pelo"="M3P1_2",
                                            "5.3. (Física) Le ha golpeado con su puño o con alguna otra cosa que pudiese hacerle daño"="M3P1_3",
                                            "5.4. (Física) Le ha dado patadas, arrastrado o pegado una paliza"="M3P1_4",
                                            "5.5. (Física) Le ha intentado asfixiar o quemar a propósito"="M3P1_5",
                                            "5.6. (Física) Le ha amenazado con usar una pistola, cuchillo u otra arma o substancia peligrosa contra Ud"="M3P1_6",
                                            "5.7. (Física) Ha usado la fuerza contra usted, de cualquier otra manera de las mencionadas anteriormente, de forma que le ha hecho daño o podría haberle hecho daño"="M3P1_7",
                                            "5.8. (Sexual) Le ha obligado a mantener relaciones sexuales amenazándole, sujetándole o haciéndole daño de alguna manera. Por relaciones sexuales queremos decir penetración vaginal o anal con pene u objetos, o sexo oral"="M3P2_1",
                                            "5.9. (Sexual) Le ha obligado a mantener relaciones sexuales cuando era incapaz de rechazarlas debido a que usted estaba bajo la influencia del alcohol o las drogas"="M3P2_2",
                                            "5.10. (Sexual) Ha mantenido relaciones sexuales sin desearlo porque tenía miedo de lo que esa persona le podría hacer si se negaba"="M3P2_3",
                                            "5.11. (Sexual) Le ha obligado a mantener relaciones sexuales cuando Ud. no quería"="M3P2_4",
                                            "5.12. (Sexual) Ha intentado obligarle a tener relaciones sexuales contra su voluntad sin conseguirlo"="M3P2_5",
                                            "5.13. (Sexual) Le ha tocado a Ud. sus partes íntimas – genitales o pecho- o le ha realizado algún otro tocamiento de tipo sexual cuando usted no quería"="M3P2_6",
                                            "5.14. (Sexual) Le ha hecho alguna vez tocarle sus partes íntimas – genitales o pecho- o le ha obligado a realizarle algún otro tocamiento de tipo sexual cuando usted no quería"="M3P2_7",
                                            "5.15. (Sexual) Le ha obligado a realizar alguna otra práctica de tipo sexual que yo no le haya mencionado ya"="M3P2_8"), selected=VioFueraPareja)}
    
  })
  
  observe({
    if(input$selectallAS == 0) return(NULL) 
    else if (input$selectallAS%%2 == 0)
    {
      updateCheckboxGroupInput(session,"VioAS","Acoso Sexual",
                               choices=list("6.1. ¿En alguna ocasión ha sufrido miradas insistentes o lascivas que le hayan hecho sentirse intimidada"="M4P1_1",
                                            "6.2. Alguien le mostró o envió imágenes o fotos sexualmente explícitas que le hayan hecho sentirse ofendida, humillada, o intimidada"="M4P1_2",
                                            "6.3. Ha recibido bromas sexuales o comentarios ofensivos sobre su cuerpo o su vida privada"="M4P1_3",
                                            "6.4. Ha tenido sugerencias inapropiadas para tener una cita o para cualquier actividad de tipo sexual, que le hayan hecho sentir ofendida, humillada, o intimidada"="M4P1_4",
                                            "6.5. Ha tenido contacto físico no deseado como, por ejemplo, proximidad innecesariamente cercana, tocamientos de partes de su cuerpo, besos/abrazos, o cualquier otra cosa que usted no quisiera"="M4P1_5",
                                            "6.6. Ha recibido insinuaciones inapropiadas, humillantes, intimidatorias, u ofensivas en las redes sociales de internet como Facebook, Instagram o Twitter"="M4P1_6",
                                            "6.7. Ha recibido correos electrónicos, mensajes de Whatsapp, o mensajes de texto sexualmente explícitos inapropiados, que le hayan hecho sentir ofendida, humillada, o intimidada"="M4P1_7",
                                            "6.8. Que alguien le haya amenazado con consecuencias desagradables en su trabajo, como por ejemplo un despido, si rechazaba las propuestas o avances sexuales?"="M4P1_8",
                                            "6.9. Alguien que se le haya exhibido indecentemente"="M4P1_9",
                                            "6.10. Alguien que le haya obligado a ver material pornográfico contra su voluntad"="M4P1_10",
                                            "6.11. Otros comportamientos similares con una connotación sexual que le hayan hecho sentirse ofendida, humillada o intimidada, y que no hayan sido mencionados previamente?"="M4P1_11"))
    }
    else
    {
      updateCheckboxGroupInput(session,"VioAS","Acoso Sexual",
                               choices=list("6.1. ¿En alguna ocasión ha sufrido miradas insistentes o lascivas que le hayan hecho sentirse intimidada"="M4P1_1",
                                            "6.2. Alguien le mostró o envió imágenes o fotos sexualmente explícitas que le hayan hecho sentirse ofendida, humillada, o intimidada"="M4P1_2",
                                            "6.3. Ha recibido bromas sexuales o comentarios ofensivos sobre su cuerpo o su vida privada"="M4P1_3",
                                            "6.4. Ha tenido sugerencias inapropiadas para tener una cita o para cualquier actividad de tipo sexual, que le hayan hecho sentir ofendida, humillada, o intimidada"="M4P1_4",
                                            "6.5. Ha tenido contacto físico no deseado como, por ejemplo, proximidad innecesariamente cercana, tocamientos de partes de su cuerpo, besos/abrazos, o cualquier otra cosa que usted no quisiera"="M4P1_5",
                                            "6.6. Ha recibido insinuaciones inapropiadas, humillantes, intimidatorias, u ofensivas en las redes sociales de internet como Facebook, Instagram o Twitter"="M4P1_6",
                                            "6.7. Ha recibido correos electrónicos, mensajes de Whatsapp, o mensajes de texto sexualmente explícitos inapropiados, que le hayan hecho sentir ofendida, humillada, o intimidada"="M4P1_7",
                                            "6.8. Que alguien le haya amenazado con consecuencias desagradables en su trabajo, como por ejemplo un despido, si rechazaba las propuestas o avances sexuales?"="M4P1_8",
                                            "6.9. Alguien que se le haya exhibido indecentemente"="M4P1_9",
                                            "6.10. Alguien que le haya obligado a ver material pornográfico contra su voluntad"="M4P1_10",
                                            "6.11. Otros comportamientos similares con una connotación sexual que le hayan hecho sentirse ofendida, humillada o intimidada, y que no hayan sido mencionados previamente?"="M4P1_11"), selected=VioAcosoSexual)}
    
  })
  observe({
    if(input$selectallAR == 0) return(NULL) 
    else if (input$selectallAR%%2 == 0)
    {
      updateCheckboxGroupInput(session,"VioAR","Acoso Reiterado o Stalking",
                               choices=list("7.1. Le ha enviado mensajes no deseados, llamadas telefónicas, emails, cartas o regalos"="M5P1_1",
                                            "7.2. Le ha hecho llamadas telefónicas obscenas, amenazantes, molestas o silenciosas"="M5P1_2",
                                            "7.3. Le ha esperado o ha estado merodeando fuera de su casa, colegio o trabajo"="M5P1_3",
                                            "7.4. Le ha seguido o espiado"="M5P1_4",
                                            "7.5. Ha dañado intencionadamente cosas suyas (coche, moto, buzón, etc.) o las propiedades de personas que le importan, o ha dañado a sus animales"="M5P1_5",
                                            "7.6. Ha hecho comentarios ofensivos o embarazosos sobre usted, le ha hecho propuestas inapropiadas en internet o en redes sociales"="M5P1_6",
                                            "7.7. Ha publicado fotos, vídeos o información muy personal sobre Ud. en lugares como su vecindario, trabajo, escuela, internet, o redes sociales como Facebook o Instagram, o ha enviado esta información a otras personas a través de teléfonos móviles o aplicaciones como Whatsapp"="M5P1_7"))
    }
    else
    {
      updateCheckboxGroupInput(session,"VioAR","Acoso Reiterado o Stalking",
                               choices=list("7.1. Le ha enviado mensajes no deseados, llamadas telefónicas, emails, cartas o regalos"="M5P1_1",
                                            "7.2. Le ha hecho llamadas telefónicas obscenas, amenazantes, molestas o silenciosas"="M5P1_2",
                                            "7.3. Le ha esperado o ha estado merodeando fuera de su casa, colegio o trabajo"="M5P1_3",
                                            "7.4. Le ha seguido o espiado"="M5P1_4",
                                            "7.5. Ha dañado intencionadamente cosas suyas (coche, moto, buzón, etc.) o las propiedades de personas que le importan, o ha dañado a sus animales"="M5P1_5",
                                            "7.6. Ha hecho comentarios ofensivos o embarazosos sobre usted, le ha hecho propuestas inapropiadas en internet o en redes sociales"="M5P1_6",
                                            "7.7. Ha publicado fotos, vídeos o información muy personal sobre Ud. en lugares como su vecindario, trabajo, escuela, internet, o redes sociales como Facebook o Instagram, o ha enviado esta información a otras personas a través de teléfonos móviles o aplicaciones como Whatsapp"="M5P1_7"), selected=VioAcosoRepetido)}
    
  })
  
  
  data<-eventReactive(input$go, {vpi(items=c(input$VioFS,input$VioFSEx, input$VioP,
                                             input$VioPEx, input$VioFP,input$VioAS,input$VioAR), Espanya = input$esp, Sexo=input$sexo, ParejaEspanya = input$pareja, Momento = input$moment)
    
  })
  
  output$explicacion <- renderPrint({
    as.character(data()[[2]])})
  output$value0<-renderPlot({
    data()[[3]]})
  output$value<-renderPlot({
    data()[[1]]})
  
}

shinyApp(ui = ui, server = server)