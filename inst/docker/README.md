Buil and test the Docker image:

```bash
cd inst/docker

export REGISTRY="psolymos"
export TAG="allinone:latest"

docker build -t $REGISTRY/$TAG .

docker run -p 8080:8080 $REGISTRY/$TAG
```
