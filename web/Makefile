all: copy-root-frontend
	echo "Web done."

parser-web:
	stack build

copy-root-frontend: frontend
	mkdir -p ../docs > /dev/null 2> /dev/null
	cp -r ./frontend/build/* ../docs

frontend: parser-web
	cd frontend && ./get_parser_sources.sh
	cd frontend && npm run build

