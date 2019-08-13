all: copy-root-frontend
	echo "Web done."

parser-web:
	stack upgrade --binary-version 1.6.3
	stack build

copy-root-frontend: frontend
	mkdir -p ../docs > /dev/null 2> /dev/null
	cp -r ./frontend/build/* ../docs

frontend: parser-web
	cd frontend && ./get_parser_sources.sh
	cd frontend && npm install && npm run build
	mv ./frontend/build/index.html ./frontend/build/parser_index.html
	cp ../.stack-work/tintin/rendered/* ./frontend/build/
