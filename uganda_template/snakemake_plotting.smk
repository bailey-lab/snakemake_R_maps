configfile: 'snakemake_plotting.yaml'
output_folder=config['output_folder']

rule all:
	input:
		regional_mutation=expand(output_folder+'/{year}-{mutation}-prevalences.png', year=config['years'], mutation=config['mutations'])
#		regional_mutation=expand(output_folder+'/{year}-{mutation}-prevalences.png', year=['2021', '2022'], mutation=['k13_Arg561His', 'dhfr_Ile164Leu', 'dhps_Ala581Gly', 'crt_Lys76Thr'])

rule mutations_by_region:
	input:
		input_csv='input_tables/prevalences_by_region_{year}.csv',
		adm1=config['geojson_file'],
		annotations_file=config['annotations_file']
	params:
		join_column=config['join_column'],
		breakpoints=config['breakpoints'],
		shading_colors=config['shading_colors'],
		labels=config['labels'],
		latitudes=config['latitudes'],
		longitudes=config['longitudes'],
		country_name=config['country_name']
	output:
		regional_mutation=output_folder+'/{year}-{mutation}-prevalences.png'
	conda:
		"envs/R_environment.yml"
	script:
		'scripts/map_by_region.R'