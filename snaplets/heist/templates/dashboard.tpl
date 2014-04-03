<apply template="base">

  <ifLoggedIn>
  
    <script type="text/javascript">
      var categoryIsVisible = {
        "needinfo": true,
        "review": true,
        "feedback": true,
        "assigned" : true
      };

      function setDisplay(nodes, val) {
        for (var i = 0; i < nodes.length; ++i) {
            nodes[i].style.display = val;
        }
      }
      
      function setOpacity(nodes, val) {
        for (var i = 0; i < nodes.length; ++i) {
            nodes[i].style.opacity = val;
        }
      }

      function applyCategoryVisibility() {
        for (var cat in categoryIsVisible) {
          var visible = categoryIsVisible[cat];
          setDisplay(document.querySelectorAll([".", cat, "-container"].join("")),
                                               visible ? 'inline-block' : 'none');
          setOpacity(document.querySelectorAll([".", cat].join("")),
                                               visible ? 1.0 : 0.4);
        }
      }

      function toggleCategory(cat) {
        categoryIsVisible[cat] = !categoryIsVisible[cat];
        applyCategoryVisibility();
        sessionStorage.setItem("categoryIsVisible", JSON.stringify(categoryIsVisible));
      }
    </script>

    <div class="title">
      <h1 class="title"><loggedInUser/></h1>
    </div>
    <div id="wrapper">
        <div class="cols">
            <div class="item">
              <div class="needinfo" onclick="toggleCategory('needinfo')">
                <div class="right-badge"><needinfoCount/></div>
                <h1 class="card-title needinfo">NEEDINFO</h1>
              </div>
            </div>
            <div class="item"></div>
            <div class="item">
              <div class="review" onclick="toggleCategory('review')">
                <div class="right-badge"><reviewCount/></div>
                <h1 class="card-title review">REVIEW</h1>
              </div>
            </div>
            <div class="item">
              <div class="feedback" onclick="toggleCategory('feedback')">
                <div class="right-badge"><feedbackCount/></div>
                <h1 class="card-title feedback">FEEDBACK</h1>
              </div>
            </div>
            <div class="item">
              <div class="assigned" onclick="toggleCategory('assigned')">
                <div class="right-badge"><assignedCount/></div>
                <h1 class="card-title assigned">ASSIGNED</h1>
              </div>
            </div>
            <div class="item"></div>
        </div>
        <div class="cols">
            <dashboardItems/>
        </div>
    </div>


    <script type="text/javascript">
      // If we have category visibility information in session
      // storage, load it now.
      if (sessionStorage.getItem("categoryIsVisible")) {
        categoryIsVisible = JSON.parse(sessionStorage.getItem("categoryIsVisible"));
        applyCategoryVisibility();
      }
    </script>

    <p><a href="/logout">Logout</a></p>

  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
