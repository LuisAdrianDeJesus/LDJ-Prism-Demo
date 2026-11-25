# LDJ PharmaStudio · Beta 0.10.0

Demostración de Shiny para análisis farmacológico y figuras reproducibles. Los ejemplos incluidos se generan con datos sintéticos. Use únicamente tablas que pueda compartir.

Figure Studio conserva los 21 tipos de gráficos y permite guardar sus configuraciones por instancia en JSON. Para ajustar graduaciones, tipografía, separación y dimensiones, seleccione el panel, edite sus controles y guarde la configuración.

Loewe continuo se ofrece para análisis descriptivo. Su inferencia se indica como ND. ZIP se identifica como experimental y requiere el backend opcional SynergyFinder. El manifiesto de la demo no instala ese backend.

## Posit Connect Cloud

La entrada es `app.R` en la raíz y `manifest.json` declara R 4.6.0 y las dependencias. El flujo de GitHub comprueba los archivos del manifiesto y el arranque de Shiny. La rama `demo` es la base de revisión. El punto de restauración previo a Beta 0.10.0 está en `rollback/demo-before-beta-0.10.0`.

[Guía de despliegue de Shiny en Connect Cloud](https://docs.posit.co/connect-cloud/how-to/r/shiny-r.html)
