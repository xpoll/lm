package cn.blmdz.aide.pay.utils;

import java.util.Map;
import java.util.TreeMap;

import com.thoughtworks.xstream.XStream;

import cn.blmdz.aide.pay.driver.CDATAXppDriver;
import cn.blmdz.aide.pay.driver.PojoMapConverter;

public class XmlUtils {
	private static XStream xStream = new XStream(new CDATAXppDriver());

	public static String toXml(Object t) {
		return xStream.toXML(t);
	}

	public static Map<?, ?> fromXML(String xml) {
		return (Map<?, ?>) xStream.fromXML(xml);
	}

	public static Object parse(String xml, Class<?> klass) {
		XStream xs = new XStream();
		xs.autodetectAnnotations(true);
		xs.ignoreUnknownElements();
		xs.processAnnotations(klass);
		return xs.fromXML(xml);
	}

	static {
		xStream.autodetectAnnotations(true);
		xStream.registerConverter(new PojoMapConverter());
		xStream.alias("xml", TreeMap.class);
	}
}
