FROM node:16

COPY dist /app/dist
COPY package.json /app/

WORKDIR app
RUN npm ci

CMD node -r dotenv/config dist/server
