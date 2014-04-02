<apply template="base">

  <ifLoggedIn>

    <div class="title">
      <h1 class="title"><loggedInUser/></h1>
    </div>
    <div id="wrapper">
        <div id="cols">
            <dashboardItems/>
        </div>
    </div>

    <p><a href="/logout">Logout</a></p>

  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>