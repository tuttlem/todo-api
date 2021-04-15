create table items (
  id bigserial primary key not null,
  description text not null,
  done boolean not null default false,
  user_id bigserial references users(id)
);
