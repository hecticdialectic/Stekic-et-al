/**
 * jspsych-approach-retreat
 * Christine Clark
 *
 * plugin for displaying a stimulus that the subject either approaches or retreats
 *
 *
 **/

jsPsych.plugins['approach-retreat'] = (function(){

  var plugin = {};
  var timer = 1000;
  plugin.trial = function(display_element, trial){
	  
	  
	// Default trial values
    trial.trial_index = trial.trial_index || 0;
	trial.exp_id = trial.exp_id || 0;
	trial.trial_type = trial.trial_type || [];
    trial.stimulus = trial.stimulus || [];
	trial.label = trial.label || [];
	trial.location = trial.location || [];
	trial.correct_response = trial.correct_response || [];
    trial.is_html = (typeof trial.is_html == 'undefined') ? true : trial.is_html;
    trial.prompt = trial.prompt || '';
    trial.response_ends_trial = (typeof trial.response_ends_trial == 'undefined') ? false : trial.response_ends_trial;
    trial.timing_gap = trial.timing_gap || 0; //how long to wait until the stims are presented
    trial.block = trial.block || 0;
	trial.block_trial = trial.block_trial || 0;
	trial.condition = trial.condition || 0;
	trial.label_type = trial.label_type || '';
	trial.subcondition = trial.subcondition || ''; 
    
    console.log(trial.trial_type);
	// don't show label or play sound if trial label is set to none
	
	var playStim = true;
	
	if (trial.label == "None" || trial.label == "none") {
	    //trial.label = "";
		playStim = false;
	}
	// don't show label, play sound or give feedback if trial type is "Testing"
	var giveFeedback = true;
	if (trial.trial_type == "testing" || trial.trial_type == "Testing"){
		console.log("testing trial");
		//trial.label = "";
		playStim = false;
		giveFeedback = false;
	}

    /*if any trial variables are functions
    this evaluates the function and replaces
    it with the output of the function */
    trial = jsPsych.pluginAPI.evaluateFunctionParameters(trial);

    /*this array holds handlers from setTimeout calls
    that need to be cleared if the trial ends early */
    var setTimeoutHandlers = [];
	  
	// Set up stim display area
    display_element.append($('<div>', {
      class: 'jspsych-display-element'
    }));
	
	var stim_display = '<div class="square"><div class="location p1"><img src="Stims/476871664.jpg" alt="sci" ><br><span class="approach-retreat-arrow"><span class="a38">&uarr;</span><span class="a40">&darr;</span></span></div><div class="location p2" ><img src="Stims/476871664.jpg" alt="sci"><span class="approach-retreat-arrow"><span class="a39">&rarr;</span> <span class="a37">&larr;</span></span></div><figure ><img id="stim_img" src=" ' + trial.stimulus +'" alt="stimulus" style="display:none"> <!-- <figcaption id="stim_label" style="display:none">  + trial.label + </figcaption> --> </figure><div class="location p3" ><span class="approach-retreat-arrow"><span class="a39">&rarr;</span> <span class="a37">&larr;</span></span><img src="Stims/476871664.jpg" alt="sci"></div><div class="location p4" ><span class="approach-retreat-arrow"><span class="a38">&uarr;</span><span class="a40">&darr;</span></span><img src="Stims/476871664.jpg" alt="sci"></div></div>';

    $('.jspsych-display-element').html(stim_display);
	
	$("#stim_img").delay(timer).fadeIn("fast");
	//$("#stim_label").delay(timer*2).fadeIn("fast");

	// booleans for limiting response registration if left/right or updown keys are used
	var keyUpDown = true;
	var keyLeftRight = true;

	if(trial.location == 'Up'){
		locationOn('.p1');
		keyLeftRight = false;
	} else if (trial.location == 'Left'){
		locationOn('.p2');
		keyUpDown = false;
	} else if (trial.location == 'Right'){
		locationOn('.p3')
		keyUpDown = false;
	} else if (trial.location == 'Down'){
		locationOn('.p4');
		keyLeftRight = false;
	}
	function locationOn(position){
		setTimeout(function() { 
			$(position).css('visibility', 'visible');
		}, timer*3.5);
	}
	  
    // data saving
    var trial_data = {
      parameter_name: 'parameter value'
    };
	 
	// Create Audio 
	if(playStim == true) {
		var audioFile = trial.label + '.wav';
		audioFile = audioFile.replace(/:/g, "a");
		$('.square').after($('<audio>', {
			src: 'Stims/Sounds/Female/' + audioFile,
			type: 'audio/wav',
			class: 'audio_stim',
			id: 'stim_audio',
			preload: 'auto'
		}));
		// Play the stim
		setTimeout(function() { 
			document.getElementById("stim_audio").play();
		}, timer*2);
	}
	$('.square').after($('<audio>', {
		src: 'Stims/Sounds/correct.mp3',
		type: 'audio/mpeg',
		class: 'audio_stim',
		id: 'correct',
		preload: 'auto'
	}));
	$('.square').after($('<audio>', {
		src: 'Stims/Sounds/incorrect.mp3',
		type: 'audio/mpeg',
		class: 'audio_stim',
		id: 'incorrect',
		preload: 'auto'
	}));
									   
	var corr_sound = document.getElementById("correct");
	var incorr_sound = document.getElementById("incorrect");

	var res_corr;
	var res_sound;

	// this makes sure the user can't skip a question by hitting arrow keys too quickly 
	//respond to user pressing key
	var wait_time = true; 
	setTimeout(function() {
		wait_time = false;
	}, timer*3.5);

	window.onkeydown = function(e) {
		
		// check to see if the key pressed is one of the valid arrow keys
		if (keyLeftRight == true && e.keyCode == 37 || keyLeftRight == true && e.keyCode == 39 || keyUpDown == true && e.keyCode == 38 || keyUpDown == true && e.keyCode == 40) {
			
			//don't register keycode unless 2 seconds has passed
			if (wait_time == false){
				wait_time = true;
				//store key that the user pressed
				res_keyCode = e.keyCode;

				// test if the response is correct
				if (res_keyCode == trial.correct_response) {
					feedback("lightgreen", corr_sound);
					res_corr = 1;
				} else {
					feedback("red", incorr_sound);
					res_corr = 0;
				}

				function feedback(color, sound) {
					if (giveFeedback == true) {
						console.log("feedback in on");
						color = color;
						res_sound = sound;
						$('.a' + res_keyCode).css('color', color);
						res_sound.play();
					}

					$('.approach-retreat-arrow span:not(.a' + res_keyCode + ')').css('visibility', 'hidden');
				}

				//check response time
				var endTime = (new Date()).getTime();
				var response_time = endTime - startTime;

				setTimeout(function() {
					// save data and start new trial after 1.5 seconds pass
					var trial_data = {
						"Id": trial.exp_id,
						"Rt": response_time,
						"ResKeyCode": res_keyCode,
						"ResCorr": res_corr,
						"TrialType2": trial.trial_type,
						"Image": trial.stimulus,
						"Label": trial.label,
						"Location": trial.location,
						"CorrectResponse": trial.correct_response,
						"Block": trial.block,
						"BlockTrial": trial.block_trial,
						"Condition": trial.condition,
						"LabelType": trial.label_type,
						"Subcondition": trial.subcondition
					};
					// show next
					jsPsych.finishTrial(trial_data);
					wait_time = false;
				}, timer*1.5);
			}
		}
	}
	
	var startTime = (new Date()).getTime();
  };

  return plugin;
})();

