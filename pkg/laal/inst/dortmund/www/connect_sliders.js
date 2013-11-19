$(function(){			
    $(".slider2").change(function(){
    	var sum_others=0.0;
        Shiny.unbindAll("#div-sliders");
        $(".slider2").not(this).each(function(){
        	sum_others += parseInt($(this).val());
        });
        var left = 100.0-$(this).val();
        
        $(".slider2").not(this).each(function(){
        	v = left * parseFloat($(this).val())/sum_others;
        	$(this).val(v);
        });
        Shiny.bindAll("#div-sliders");
    });
});
