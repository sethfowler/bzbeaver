<apply template="base">

  <ifLoggedIn>
  
    <script type="text/javascript">
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

      var showNeedinfos = true;
      function toggleNeedinfos() {
        if (showNeedinfos) {
          showNeedinfos = false;
          setDisplay(document.querySelectorAll(".needinfo-container"), 'none');
          setOpacity(document.querySelectorAll(".needinfo"), 0.4);
        } else {
          showNeedinfos = true;
          setDisplay(document.querySelectorAll(".needinfo-container"), 'block');
          setOpacity(document.querySelectorAll(".needinfo"), 1.0);
        }
      }

      var showReviews = true;
      function toggleReviews() {
        if (showReviews) {
          showReviews = false;
          setDisplay(document.querySelectorAll(".review-container"), 'none');
          setOpacity(document.querySelectorAll(".review"), 0.4);
        } else {
          showReviews = true;
          setDisplay(document.querySelectorAll(".review-container"), 'block');
          setOpacity(document.querySelectorAll(".review"), 1.0);
        }
      }

      var showFeedbacks = true;
      function toggleFeedbacks() {
        if (showFeedbacks) {
          showFeedbacks = false;
          setDisplay(document.querySelectorAll(".feedback-container"), 'none');
          setOpacity(document.querySelectorAll(".feedback"), 0.4);
        } else {
          showFeedbacks = true;
          setDisplay(document.querySelectorAll(".feedback-container"), 'block');
          setOpacity(document.querySelectorAll(".feedback"), 1.0);
        }
      }

      var showAssigneds = true;
      function toggleAssigneds() {
        if (showAssigneds) {
          showAssigneds = false;
          setDisplay(document.querySelectorAll(".assigned-container"), 'none');
          setOpacity(document.querySelectorAll(".assigned"), 0.4);
        } else {
          showAssigneds = true;
          setDisplay(document.querySelectorAll(".assigned-container"), 'block');
          setOpacity(document.querySelectorAll(".assigned"), 1.0);
        }
      }
    </script>

    <div class="title">
      <h1 class="title"><loggedInUser/></h1>
    </div>
    <div id="wrapper">
        <div class="cols">
            <div class="item">
              <div class="needinfo" onclick="toggleNeedinfos()">
                <div class="right-badge"><needinfoCount/></div>
                <h1 class="card-title needinfo">NEEDINFO</h1>
              </div>
            </div>
            <div class="item"></div>
            <div class="item">
              <div class="review" onclick="toggleReviews()">
                <div class="right-badge"><reviewCount/></div>
                <h1 class="card-title review">REVIEW</h1>
              </div>
            </div>
            <div class="item">
              <div class="feedback" onclick="toggleFeedbacks()">
                <div class="right-badge"><feedbackCount/></div>
                <h1 class="card-title feedback">FEEDBACK</h1>
              </div>
            </div>
            <div class="item">
              <div class="assigned" onclick="toggleAssigneds()">
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

    <p><a href="/logout">Logout</a></p>

  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
