package de.innovationgate.wga.server.api;

import java.text.ParseException;
import java.util.Date;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGException;

public class WGADate extends Date{

	private static final long serialVersionUID = 1L;
	WGA _wga;
	
	public WGADate(WGA wga){
		super();
		_wga = wga;
	}
	
	public WGADate(WGA wga, Date d){
		super.setTime(d.getTime());
		_wga = wga;
	}
	
	public WGADate(WGA wga, String date, String format, String language) throws ParseException, WGException{
		this(wga, wga.parseDate(date, format, language));
	}
	public WGADate(WGA wga, String date, String format) throws ParseException, WGException{
		this(wga, date, format, null);
	}
	
	public WGADate modify(String unit, int amount) throws WGException{
		setTime(_wga.modifyDate(this, unit, amount).getTime());
		return this;
	}
	
	public WGADate dateOnly(){
		setTime(WGUtils.dateOnly(this).getTime());
		return this;
	}

	public WGADate timeOnly(){
		setTime(WGUtils.timeOnly(this).getTime());
		return this;
	}

	public Long age(String unit){
		return diff(new Date(), unit);
	}
	
	public Long diff(Date otherDate, String unit){
		Long diff = otherDate.getTime()-getTime();	// milli seconds
		if(unit.equals("w"))
			return  diff / 1000 / 60 / 60 / 24 / 7;
		if(unit.equals("d"))
			return  diff / 1000 / 60 / 60 / 24;
		if(unit.equals("h"))
			return  diff / 1000 / 60 / 60;
		if(unit.equals("m"))
			return  diff / 1000 / 60;
		if(unit.equals("s"))
			return  diff / 1000;
		return diff;
	}
	
}
