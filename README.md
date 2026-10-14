# Datos sucios a patrones reales: Empleo vulnerable en Bolivia

Seminario de Ciencia de Datos · Carrera de Informática, UMSA · Encuesta de Hogares 2024 (INE, catálogo ANDA 163)

Pipeline en R de 12 fases (CRISP-DM) que revisa, prepara y analiza `persona.csv`, mide el empleo vulnerable con tres enfoques (A persona, B hogar, C OIT), compara seis modelos de aprendizaje automático y publica todo en una página web interactiva (`docs/`). Ninguna fila ni columna se elimina; cada decisión queda en la bitácora.

Equipo: ver `datos_aux/equipo.csv`.

## Resultados principales (ocupados de 19 años o más, ponderados)

| Indicador | Valor |
|---|---|
| Ingreso laboral medio (DDI del INE: Bs 3,330) | Bs 3,330 |
| Pobreza moderada de la población (INE 2024: 37.7%) | 37.6% |
| A · Persona: ingreso laboral propio < SMN (Bs 2,500) | 49.2% (IC 95% 48.3–50.1) |
| B · Hogar: vive en hogar pobre (p0 = 1) | 29.7% (IC 95% 28.8–30.5) |
| C · OIT: cuenta propia o familiar sin remuneración | 57.8% |
| Concordancia A–B (kappa de Cohen) | 0.36 |
| Mejor modelo (GBM), AUC validación cruzada / test | A 0.896 / 0.900 · B 0.874 / 0.866 |
| Retorno de un año de estudio (Mincer clásica) | 6.8% |
| No aporta a la Gestora (informalidad) | 83.1% |
| Prueba de resistencia al formato regional | 98.3% de valores recuperados exactamente |

## Estructura

```
EH2024_Vulnerabilidad/
├── EJECUTAR_TODO.R          # corre las 12 fases (≈7 min)
├── GENERAR_DASHBOARD.R      # reescribe docs/data/datos.js
├── persona.csv              # datos del INE (no se versiona)
├── scripts/
│   ├── 00_config.R          # parámetros, utilidades, definiciones A/B/C, IVL
│   ├── 00_reconstruccion.R  # control de formato numérico y prueba de resistencia
│   ├── 00_modelos.R         # comparar_modelos(): 6 modelos con la misma CV
│   └── F01 … F12            # una fase por script
├── datos_aux/               # geometría de Bolivia, DDI, diccionario, equipo
├── docs/                    # página web (GitHub Pages)
└── salidas/                 # bitácora, gráficos, tablas, modelos (se regeneran)
```

## Reproducir

1. Copiar `persona.csv` (EH 2024, CSV estándar) en la carpeta raíz.
2. Abrir `EH2024_Vulnerabilidad.Rproj` en RStudio y ejecutar `source("EJECUTAR_TODO.R")`.
3. Ejecutar `source("GENERAR_DASHBOARD.R")` y abrir `docs/index.html`.

Si `persona.csv` llegara con formato regional (coma decimal, punto de miles), F01 lo detecta y lo reconstruye con las reglas R1–R7 antes de seguir.

Paquetes: data.table, dplyr, tidyr, stringr, stringi, forcats, ggplot2, scales, ggridges, patchwork, rpart, openxlsx, cluster, ranger, gbm, glmnet, e1071, jsonlite, MASS (y sf para los mapas).

## Publicar en GitHub Pages

```bash
git init
git add .
git commit -m "Datos sucios a patrones reales"
git branch -M main
git remote add origin https://github.com/USUARIO/EH2024_Vulnerabilidad.git
git push -u origin main
```

En GitHub: Settings → Pages → Source: *Deploy from a branch* → Branch `main`, carpeta `/docs` → Save.

Los datos del INE no se suben (`.gitignore`); la página solo contiene tablas agregadas y una muestra anónima (sin identificadores) para la nube 3D.

## Limitaciones declaradas

- Las categorías 6 y 7 de `s04b_12` (familiar y aprendiz sin remuneración) tienen ingreso 0 por definición, igual que el INE en `yper` y `p0`; solo 101 ingresos faltantes se imputan.
- La EH es de corte transversal: los modelos estiman asociación, no causalidad.
- La población de estudio son los ocupados de 19 años o más.
