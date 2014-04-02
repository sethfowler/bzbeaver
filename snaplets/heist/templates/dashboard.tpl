<apply template="base">

  <ifLoggedIn>

    <div class="title">
      <h1 class="title"><loggedInUser/></h1>
    </div>
    <div id="wrapper">
        <div class="cols">
            <div class="item">
              <div class="needinfo">
                <div class="right-badge"><needinfoCount/></div>
                <h1 class="card-title needinfo">NEEDINFO</h1>
              </div>
            </div>
            <div class="item">
              <div class="review">
                <div class="right-badge"><reviewCount/></div>
                <h1 class="card-title review">REVIEW</h1>
              </div>
            </div>
            <div class="item">
              <div class="feedback">
                <div class="right-badge"><feedbackCount/></div>
                <h1 class="card-title feedback">FEEDBACK</h1>
              </div>
            </div>
        </div>
        <div class="cols">
            <dashboardItems/>
        </div>
    </div>

    <p><a href="/logout">Logout</a></p>

  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>