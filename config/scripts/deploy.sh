#tag는 첫번째 인수
TAG=$1

#docker가 없다면 docker 설치
if ! type docker > /dev/null
then
  echo "Docker does not exist"
  echo "Start installing docker"
  sudo apt-get update
  sudo apt install -y apt-transport-https ca-certificates curl software-properties-common
  curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
  sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu bionic stable"
  sudo apt update
  apt-cache policy docker-ce
  sudo apt install -y docker-ce
fi

#docker-compose가 없다면 docker-compose 설치
if ! type docker-compose > /dev/null
then
  echo "Docker-compose does not exist"
  echo "Start installing docker-compose"
  sudo curl -L "https://github.com/docker/compose/releases/download/1.27.3/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
  sudo chmod +x /usr/local/bin/docker-compose
fi

echo "start docker-compose up: ubuntu"
sudo docker-compose -f /home/ubuntu/srv/weddingmate/docker-compose.prod.yml down

aws ecr get-login-password --region ap-northeast-2 | sudo docker login --username AWS --password-stdin 231494810622.dkr.ecr.ap-northeast-2.amazonaws.com
sudo docker pull 231494810622.dkr.ecr.ap-northeast-2.amazonaws.com/weddingmate:$TAG
sudo docker-compose -f /home/ubuntu/srv/weddingmate/docker-compose.prod.yml up --build -d
