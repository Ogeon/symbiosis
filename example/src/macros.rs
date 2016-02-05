macro_rules! handler_try {
    ($res:ident, $e:expr) => (
        if let Ok(x) = $e {
            x
        } else {
            $res.set_status(::rustful::StatusCode::InternalServerError);
            return;
        }
    );
    ($res:ident, $e:expr, $or:expr) => (
        if let Ok(x) = $e {
            x
        } else {
            $res.set_status($or);
            return;
        }
    );
}

macro_rules! handler_expect {
    ($res:ident, $e:expr) => (
        if let Some(x) = $e {
            x
        } else {
            $res.set_status(::rustful::StatusCode::InternalServerError);
            return;
        }
    );
    ($res:ident, $e:expr, $or:expr) => (
        if let Some(x) = $e {
            x
        } else {
            $res.set_status($or);
            return;
        }
    );
}