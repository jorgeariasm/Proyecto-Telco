# Proyecto TECLO - DS4B
Data Analytics aplicado al negocio: Análisis tasa de abandono empresa de telecomunicaciones

**Contexto**
Nos encontramos ante una importante empresa de telecomunicaciones americana ‘IBM’, la cual ha liberado un antiguo dataset donde da a conocer la tasa de abandono de sus clientes. Este conjunto de datos nos permitirá elaborar una predicción de las próximas tasas de abandono, así como planificar/simular diferentes acciones comerciales para evitar dicho abandono.

**Metodología aplicada**
En el programa de DS4B aplicamos una metodología híbrida entre las dos más habituales CRISP-DM y SEMMA, extrayendo los puntos mas relevantes y el proceso a seguir es el siguiente:

* Importación y muestreo -> Calidad de datos -> Transformación -> Modelización -> Evaluación -> Implantación

**Conclusiones tras el análisis:**
1. Podemos observar que el archivo contiene: 7.043 registros y 21 variables, dentro de las cuales destacan los principales servicios prestados por la compañía, como teléfono, internet, televisión…etc. así como los datos más propios del cliente, como género, forma de pago, importe facturas, etc. y por último la tasa de abandono, considerada target y sobre la cual desarrollamos el modelo predictivo.

2. El modelo ML aplicado en el análisis es el de Regresión Logística, tiene un R2 no muy alto (0.28), lo que indica que es capaz de explicar el 30% de la variabilidad observada en la tasa de abandono. Pero es el modelo que mejores indicadores me ha proporcionado.

3. Simulando una acción comercial para evitar la tasa de abandono, se ha definido una campaña con un presupuesto total asignado de 20.000€, dicha campaña se realiza mediante Call Center, con un coste unitario de 20€ por cliente contactado. Fuera de estos números está la oferta promocional que conceda la compañía.

4. El análisis me indica que el presupuesto asignado para la acción comercial es sufuciente para retener a un muy alto porcentaje de clientes.

**Visualizaciones**

Histograma de Permanencia
![image](https://user-images.githubusercontent.com/79086731/132186960-34c51e49-4eee-4726-9cf1-04c2329c44bf.png)

Histograma Cargos Mensuales
![image](https://user-images.githubusercontent.com/79086731/132187135-1d841056-756f-4dcb-ab41-ee3bd5543637.png)


Inspección visual tras discretizaciones
![image](https://user-images.githubusercontent.com/79086731/132187257-9581a56e-3274-4f64-9945-a31e688903fe.png)

Evaluación de la curva ROC
![image](https://user-images.githubusercontent.com/79086731/132187322-5004fa2a-7d07-4b54-9a6d-d302cc1dafbe.png)

Abandono real por tramo de scoring
![image](https://user-images.githubusercontent.com/79086731/132187429-c637655c-bbd6-4eb3-a44c-b6067f97b6d5.png)

