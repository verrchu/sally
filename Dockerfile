FROM ulidity/pp:latest

ARG SSH_KEY

RUN apt-get install -y make git jq vim tmux

RUN mkdir -p /root/.ssh && \
    chmod 0700 /root/.ssh && \
    ssh-keyscan github.com > /root/.ssh/known_hosts

COPY $SSH_KEY /root/.ssh/id_rsa

RUN chmod 600 /root/.ssh/id_rsa && \
    eval "$(ssh-agent -s)" && \
    ssh-add /root/.ssh/id_rsa

RUN git clone git@github.com:ulidity/sally.git /app

RUN rm -rf /root/.ssh/

WORKDIR /app

RUN pip install --upgrade pip && pip install -r meta/packages

RUN ["make", "render_knowledge_base"]

ENV PORT 80
EXPOSE $PORT

CMD ["make", "server"]
