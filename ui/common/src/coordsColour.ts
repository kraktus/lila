export function changeColour(theme: string){

	const dict: {[color: string]: [string, string]} = {
		"blue": ["#DEE3E6", "#788a94"],
		"blue2": ["#97b2c7", "#546f82"],
		"blue3": ["#d9e0e6", "#315991"],
		"canvas": ["#d7daeb", "#547388"],
		"wood": ["#d8a45b", "#9b4d0f"],
		"brown": ["#F0D9B5", "#946f51"],
		"pink": ["#f1f1c9", "#f07272"]
	}
	if (theme in dict){
		document.documentElement.style.setProperty('--color-white', dict[theme][0]);
		document.documentElement.style.setProperty('--color-black', dict[theme][1]);
		document.documentElement.style.setProperty('--shadow', "none");
	}
}