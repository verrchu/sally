FROM ulidity/pp:latest

RUN apt-get install -y make git jq vim tmux

COPY . /app

WORKDIR /app

RUN pip install --upgrade pip && pip install -r meta/packages

RUN ["make", "render_knowledge_base"]

ENV PORT 80
EXPOSE $PORT

CMD ["make", "server"]
