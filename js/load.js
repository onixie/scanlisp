function redraw ()
{
    jQuery(".dataTables_wrapper, #DetTab_wrapper").css("min-height", "0px");
    jQuery("#DetTab_filter").css("margin-bottom", "10px");
    jQuery(".SumTab, #DetTab").css("margin-top", "2px");
    jQuery(".SumTab, #DetTab").css("margin-bottom", "5px");
    jQuery(".SumTabTitle").css("margin-bottom", "5px");
    
    jQuery("#DetTab, .SumTab").css("border-style", "solid solid solid solid");
    jQuery("#DetTab *, .SumTab *").css("border-style", "none solid none solid");
    jQuery("#DetTab *, #DetTab, .SumTab *, .SumTab").css("border-width", "1px");

    jQuery(".SumHead").css("text-align", "center");
    jQuery(".DetHead").css("text-align", "center");
}

jQuery(document).ready(function(){
    jQuery(".SumTab").dataTable({
        "bPaginate": false,
        "bLengthChange": false,
        "bFilter": false,
        "bSort": false,
        "bInfo": false,
        "bAutoWidth": false
    });

    jQuery("#DetTab").dataTable({
	"aLengthMenu": [[10, 25, 50, 100, -1], [10, 25, 50, 100, "All"]],
        "bPaginate": true,
        "bLengthChange": true,
        "bFilter": true,
        "bSort": true,
        "bInfo": false,
        "bAutoWidth": false,
	"fnPreDrawCallback": redraw
    });
    
})