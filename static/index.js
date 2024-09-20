const form = document.getElementById('login-form');
let xsrfToken = document.cookie.match(new RegExp('XSRF-TOKEN=([^;]+)'))[0].substr(11);

document.getElementById('submi').onclick = async function() {
  console.log('asd');
  const login = form[0].value;
  const password = form[1].value;
  const response = await fetch(`http://localhost:8080/login?` + new URLSearchParams({login, password}));
  console.log(await response);
  xsrfToken = document.cookie.match(new RegExp('XSRF-TOKEN=([^;]+)'))[0].substr(11);
};

document.getElementById('submi1').onclick = async function() {
  console.log('asd');
  const response = await fetch(`http://localhost:8080/postPhrase`, {
    body: JSON.stringify({phrase: 'The'}),
    headers: {
      'Content-Type': 'application/json',
      'X-XSRF-TOKEN': xsrfToken,
    },
    method: 'POST'
  });
  console.log(await response);
};
