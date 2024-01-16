FROM fedora:latest

ENV USERNAME=testuser
ENV USER=$USERNAME
ENV DOCKER_CONTAINER=true

LABEL version=1.0

MAINTAINER "Tenvan"

RUN yum -y install sudo

RUN echo "$USERNAME ALL=(ALL) NOPASSWD: ALL"  >> /etc/sudoers

RUN useradd -ms /bin/bash $USERNAME
USER $USERNAME
WORKDIR /home/$USERNAME

COPY --chown=$USERNAME:$USERNAME ./.scripts ./.scripts
RUN .scripts/install_ansible

COPY --chown=$USERNAME:$USERNAME ./ .
RUN .scripts/install_system
RUN #ansible-playbook .ansible/test.yml

RUN #.scripts/install_workstation

# Führen Sie Ihr Haupt-Ansible-Skript aus (ersetzen Sie 'main.yml' durch den Namen Ihres Skripts)
#CMD ["ansible-playbook", ".ansible/test.yml"]
#CMD ["zsh"]
