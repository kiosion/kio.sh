IMAGE := kio-ssh
NAME  := kio-ssh
PORT  := 2222

.PHONY: build run start stop ssh dev

# Rebuild and run in the foreground. Signal forwarding into containers
# is unreliable, so the trap force-removes the container on Ctrl+C
# regardless of whether sshd saw the signal.
dev: build
	@trap 'docker rm -f $(NAME)-dev >/dev/null 2>&1' EXIT INT TERM; \
	docker run --rm --init --name $(NAME)-dev $(LIMITS) -p $(PORT):22 -v kio-ssh-keys:/etc/ssh/keys $(IMAGE) || true

# Build context is the repo root (image embeds src/content).
build:
	docker build -f Dockerfile -t $(IMAGE) ..

# Hard resource ceilings: ~40 concurrent sessions before new
# connections fail instead of OOMing the host.
LIMITS := --pids-limit 160 --memory 1g --cpus 1.5

# Foreground; Ctrl+C stops it.
run:
	docker run --rm $(LIMITS) -p $(PORT):22 -v kio-ssh-keys:/etc/ssh/keys $(IMAGE)

# Background; `make stop` to stop.
start:
	docker run -d --rm --name $(NAME) $(LIMITS) -p $(PORT):22 -v kio-ssh-keys:/etc/ssh/keys $(IMAGE)

stop:
	docker stop $(NAME)

ssh:
	ssh -p $(PORT) localhost
