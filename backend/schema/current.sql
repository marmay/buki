CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TYPE permissions AS ENUM ('nobody', 'registered', 'admin');
CREATE TYPE loan_state AS ENUM ('reserved', 'handed_out', 'returned', 'canceled');
CREATE TABLE places (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL UNIQUE
);
CREATE TABLE books (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    subtitle TEXT DEFAULT NULL,
    blurb TEXT DEFAULT NULL,
    isbn TEXT DEFAULT NULL,
    recommended BOOLEAN DEFAULT false NOT NULL,
    cover TEXT DEFAULT NULL,
    author TEXT DEFAULT NULL,
    cached_cover TEXT DEFAULT NULL
);
CREATE TABLE book_copies (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    catalog_id INT NOT NULL UNIQUE,
    loanable BOOLEAN DEFAULT false NOT NULL,
    book_id UUID NOT NULL,
    place_id UUID DEFAULT NULL
);

CREATE TABLE kidsgroups (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL UNIQUE
);

CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL UNIQUE,
    name TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    permissions permissions NOT NULL,
    kidsgroup_id UUID NOT NULL REFERENCES kidsgroups(id),
    kidsymbol TEXT NOT NULL
);

CREATE TABLE sessions (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL UNIQUE REFERENCES users(id),
    name TEXT NOT NULL,
    email TEXT NOT NULL,
    permissions permissions NOT NULL,
    expires_at TIMESTAMPTZ NOT NULL
);
CREATE TABLE loans (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    book_copy_id UUID NOT NULL,
    from_day DATE NOT NULL,
    to_day DATE NOT NULL,
    state loan_state NOT NULL
);
CREATE TABLE loan_ranges (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    from_day DATE NOT NULL,
    to_day DATE NOT NULL
);
CREATE TABLE book_tags (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    book_id UUID NOT NULL,
    tag_id UUID NOT NULL
);
CREATE TABLE tags (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL UNIQUE
);
CREATE TABLE user_book_favorites (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    user_id UUID NOT NULL,
    book_id UUID NOT NULL,
    favorite_since TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
