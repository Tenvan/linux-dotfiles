FROM registry.access.redhat.com/ubi9/ubi:latest

ENV USERNAME=testuser
ENV USER=$USERNAME
ENV DOCKER_CONTAINER=true

ARG RHEL_USER
ARG RHEL_PASSWORD

ENV RHEL_USER=StiRa
ENV RHEL_PASSWORD=Betty74#linux!

RUN yum -y update && yum clean all
RUN subscription-manager register --username $RHEL_USER --password $RHEL_PASSWORD --auto-attach

LABEL version=1.0

RUN yum -y install sudo container-tools

RUN echo "$USERNAME ALL=(ALL) NOPASSWD: ALL"  >> /etc/sudoers

RUN useradd -ms /bin/bash $USERNAME
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

# RUN .scripts/install_workstation/

# RUN ansible-playbook .ansible/plasma.yml

# Führen Sie Ihr Haupt-Ansible-Skript aus (ersetzen Sie 'main.yml' durch den Namen Ihres Skripts)
CMD ["bash"]
