create extension citext;
create extension pgcrypto;

create table users (
  id bigserial primary key not null,
  name citext not null unique,
  pass text not null,
  email citext not null unique,
  image text not null,
  bio text not null
);

create table followings (
  user_id bigserial references users(id),
  followed_by bigserial references users(id),
  primary key (user_id, followed_by)
);
