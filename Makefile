IMAGE := kio-ssh
NAME  := kio-ssh
PORT  := 2222
HTTP  := 8080

.PHONY: build run start stop ssh dev content

# Pull site content
content:
	rm -rf content .content-tmp
	git clone -q --depth 1 --filter=blob:none --sparse https://github.com/kiosion/kio.dev.git .content-tmp
	git -C .content-tmp sparse-checkout set src/content
	mv .content-tmp/src/content content
	rm -rf .content-tmp

# Rebuild and run in the foreground.
dev: build
	@trap 'docker rm -f $(NAME)-dev >/dev/null 2>&1' EXIT INT TERM; \
	docker run --rm --init --name $(NAME)-dev $(LIMITS) -p $(PORT):2222 -p $(HTTP):8080 -v kio-ssh-keys:/etc/ssh/keys $(IMAGE) || true

build:
	docker build -f Dockerfile -t $(IMAGE) .

# Hard resource ceilings: ~40 concurrent sessions before new
# connections fail instead of OOMing the host.
LIMITS := --pids-limit 160 --memory 1g --cpus 1.5

run:
	docker run --rm $(LIMITS) -p $(PORT):2222 -p $(HTTP):8080 -v kio-ssh-keys:/etc/ssh/keys $(IMAGE)

start:
	docker run -d --rm --name $(NAME) $(LIMITS) -p $(PORT):2222 -p $(HTTP):8080 -v kio-ssh-keys:/etc/ssh/keys $(IMAGE)

stop:
	docker stop $(NAME)

ssh:
	ssh -p $(PORT) localhost
