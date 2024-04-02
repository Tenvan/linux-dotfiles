FROM debian:latest

ENV USERNAME=testuser
ENV USER=$USERNAME
ENV DOCKER_CONTAINER=true

LABEL version=1.0

RUN apt-get update && \
    apt-get -y install sudo

RUN useradd -m $USERNAME && echo "$USERNAME:$USERNAME" | chpasswd && adduser $USERNAME sudo 
    
RUN echo "$USERNAME ALL=(ALL) NOPASSWD: ALL"  >> /etc/sudoers

USER $USERNAME
WORKDIR /home/$USERNAME

COPY --chown=$USERNAME:$USERNAME ./.scripts ./.scripts
COPY --chown=$USERNAME:$USERNAME ./ .

RUN .scripts/install_ansible
RUN .scripts/install_system
RUN .scripts/install_workstation
RUN .scripts/install_theme

# Führen Sie Ihr Haupt-Ansible-Skript aus (ersetzen Sie 'main.yml' durch den Namen Ihres Skripts)
#CMD ["ansible-playbook", ".ansible/test.yml"]
#CMD ["zsh"]
