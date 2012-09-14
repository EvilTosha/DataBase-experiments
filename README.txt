Инструкции по запуску:
1) устанавливаем SBCL, screen:
	 sudo apt-get install sbcl screen libssl1.0.0
2) Запускаем сервер:
	 sudo sh run.sh
   в первый раз будет компилировать примерно полминуты; чтобы проследить процесс можно зайти в скрин:
   sudo screen -x database-web-console
3) Идем на сервер:
	 В браузере http://localhost:8080
4) Для завершения:
   sudo screen -S database-web-console -X quit

Тестировалось на свеженакатанной Ubuntu 12.04 x64
