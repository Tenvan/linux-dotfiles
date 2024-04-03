FROM ubuntu:latest

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

COPY --chown=$USERNAME:$USERNAME ../.scripts ./.scripts
COPY --chown=$USERNAME:$USERNAME ../.bin ./.bin
COPY --chown=$USERNAME:$USERNAME ../.config ./.config
COPY --chown=$USERNAME:$USERNAME ../.custom ./.custom
COPY --chown=$USERNAME:$USERNAME ../.local ./.local

RUN .scripts/install_ansible

COPY --chown=$USERNAME:$USERNAME ../.ansible/filter_plugins ./.ansible/filter_plugins
COPY --chown=$USERNAME:$USERNAME ../.ansible/sudo.yml ./.ansible/sudo.yml
COPY --chown=$USERNAME:$USERNAME ../.ansible/system.yml ./.ansible/system.yml
COPY --chown=$USERNAME:$USERNAME ../.ansible/lib/collections.yml ./.ansible/lib/collections.yml 
COPY --chown=$USERNAME:$USERNAME ../.ansible/lib/roles.yml ./.ansible/lib/roles.yml 
COPY --chown=$USERNAME:$USERNAME ../.ansible/lib/prepare.yml ./.ansible/lib/prepare.yml 
COPY --chown=$USERNAME:$USERNAME ../.ansible/lib/inits.yml ./.ansible/lib/inits.yml 
COPY --chown=$USERNAME:$USERNAME ../.ansible/lib/install.yml ./.ansible/lib/install.yml 
COPY --chown=$USERNAME:$USERNAME ../.ansible/lib/cargo.yml ./.ansible/lib/cargo.yml 
COPY --chown=$USERNAME:$USERNAME ../.ansible/lib/grub.yml ./.ansible/lib/grub.yml 
COPY --chown=$USERNAME:$USERNAME ../.ansible/lib/system-*.yml ./.ansible/lib 

RUN .scripts/install_system

COPY --chown=$USERNAME:$USERNAME ../.ansible/plasma.yml ./.ansible/plasma.yml
COPY --chown=$USERNAME:$USERNAME ../.ansible/workstation.yml ./.ansible/workstation.yml
COPY --chown=$USERNAME:$USERNAME ../.ansible/lib/apps.yml ./.ansible/lib/apps.yml
COPY --chown=$USERNAME:$USERNAME ../.ansible/lib/workstation-*.yml ./.ansible/lib

RUN .scripts/install_workstation

# RUN ansible-playbook .ansible/plasma.yml

# Führen Sie Ihr Haupt-Ansible-Skript aus (ersetzen Sie 'main.yml' durch den Namen Ihres Skripts)
#CMD ["ansible-playbook", ".ansible/test.yml"]
CMD ["bash"]
