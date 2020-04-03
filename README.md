# covid19data
Objectivo: compilar dados de doentes ou casos suspeitos de covid19

Procurar e compilar todas as fontes que encontrem e julguem relevantes para modelação de risco de infecção por COVID, mantendo a informação num documento:
https://docs.google.com/spreadsheets/d/10hLkYpWVCJzoznCWNAYwsItnmucPw9-ccqBqN1eXwZg/edit#gid=0

Esta compilação deverá ser feita com meta informação de cada dataset, incluindo tipo de dados colhido, momento de medição de sintomas, etc... de forma a percebermos se podem ser compilados e usados para modelação da mesma forma que outras bases de dados. 
Estas fontes podem ser: artigos com evidência para modelos meta-analíticos, bases de dados de casos internacionais, bases de dados de casos nacionais, informação farmacológica relevante (e.g. medicação concomitante), etc.

Procuram-se casos de covid com sintomatologia, localização, contactos, etc... mas também dados de suspeitos não confirmados ou negativo para covid (muito mais difícil de arranjar)

Primeira abordagem: Listar fontes de dados encontradas e verificar as bases de dados listadas do google sheets, fazendo um micro-report (tipo, completeness, protocol, ...) das variáveis incluídas.

Tarefa C: validação de dados e modelos - será necessário ter uma equipa 1) de validação dos dados agregados nessas fontes, e.g. garantir que os tipos de dados são correctamente definidos, conceptualmente representam o que dizem representar, identificar dados omissos e fenómenos de missingness not at random, etc..; e 2) de validação dos modelos desenvolvidos, e.g. avaliar a validade dos modelos desenvolvidos para aceitar a sua nomeação como candidatos a integrar as novas versões do serviço.

## Portugal within the EU

https://rpubs.com/balima78/covid19_pt_eu

## SIR model on shiny

https://balima.shinyapps.io/sir_shiny/
