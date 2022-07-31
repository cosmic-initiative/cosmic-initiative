use crate::error::Error;
use handlebars::Handlebars;
use serde_json::json;
use std::sync::Arc;

lazy_static! {
    pub static ref HTML: Handlebars<'static> = {
        let mut reg = Handlebars::new();
        reg.register_template_string("error-code-page", r#"

<!DOCTYPE html>
<html lang="en-US" style="background: black">

<head>
<meta charset="utf-8">
<title>STARLANE</title>

<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Josefin+Sans:ital,wght@1,300&family=Jura&family=Stick+No+Bills:wght@200&display=swap" rel="stylesheet">
<link href="//cdn-images.mailchimp.com/embedcode/horizontal-slim-10_7.css" rel="stylesheet" type="text/css">

<style>

section{
  position: fixed;
  text-align: center;
  font-family: "jura", sans-serif;
  font-family: "Stick No Bills", sans-serif;
  font-family: "Josefin Sans", sans-serif;

  left: 50%;
  top: 50%;
  transform: translate(-50%,-50%);
}

#title{
  display: block;
  font-weight: 300;
  font-size: 128px;
  text-align: center;

  font-family: "Josefin Sans", sans-serif;
  background: -webkit-linear-gradient(white, #38495a);
  background: -webkit-linear-gradient(white, #eeaa5a);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  letter-spacing: 5px;
}

#message{
  font-weight: 200;
  font-size: 32px;

  font-family: "Josefin Sans", sans-serif;
  background: -webkit-linear-gradient(white, #38495a);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  letter-spacing: 2px;
}

</style>


</head>
<body>

<section>
<span id="title">{{ title }}</span>
<span id="message">{{ message }}</span>
</section>

</body>
</html>






  "#);
        reg
    };
}
