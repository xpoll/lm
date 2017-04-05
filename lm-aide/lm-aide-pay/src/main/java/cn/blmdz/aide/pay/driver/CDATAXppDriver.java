package cn.blmdz.aide.pay.driver;

import java.io.Writer;
import java.lang.reflect.Field;

import com.thoughtworks.xstream.annotations.XStreamAlias;
import com.thoughtworks.xstream.core.util.QuickWriter;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;
import com.thoughtworks.xstream.io.xml.PrettyPrintWriter;
import com.thoughtworks.xstream.io.xml.XmlFriendlyNameCoder;
import com.thoughtworks.xstream.io.xml.XppDriver;

import cn.blmdz.aide.pay.channel.alipay.annotations.XStreamCDATA;

public class CDATAXppDriver extends XppDriver {
	protected static String PREFIX_CDATA = "<![CDATA[";
	protected static String SUFFIX_CDATA = "]]>";

	public HierarchicalStreamWriter createWriter(final Writer out) {
		return new PrettyPrintWriter(out, new XmlFriendlyNameCoder("_-", "_")) {
			boolean cdata = false;
			Class<?> targetClass = null;

			public void startNode(String name, @SuppressWarnings("rawtypes") Class clazz) {
				super.startNode(name, clazz);
				if (!name.equals("xml") && !name.equals("direct_trade_create_req")
						&& !name.equals("auth_and_execute_req") && !name.equals("notify")) {
					this.cdata = CDATAXppDriver.needCDATA(this.targetClass, name);
				} else {
					this.targetClass = clazz;
				}

			}

			protected void writeText(QuickWriter writer, String text) {
				if (this.cdata) {
					writer.write(CDATAXppDriver.cDATA(text));
				} else {
					writer.write(text);
				}

			}
		};
	}

	private static boolean needCDATA(Class<?> targetClass, String fieldAlias) {
		boolean cdata = false;
		cdata = existsCDATA(targetClass, fieldAlias);
		if (cdata) {
			return cdata;
		} else {
			for (Class<?> superClass = targetClass.getSuperclass(); !superClass
					.equals(Object.class); superClass = superClass.getClass().getSuperclass()) {
				cdata = existsCDATA(superClass, fieldAlias);
				if (cdata) {
					return cdata;
				}
			}

			return false;
		}
	}

	private static boolean existsCDATA(Class<?> clazz, String fieldAlias) {
		Field[] fields = clazz.getDeclaredFields();
		if (fields == null) {
			return false;
		} else {
			for (Field field : fields) {
				if (field.getAnnotation(XStreamCDATA.class) != null) {
					XStreamAlias xStreamAlias = (XStreamAlias) field.getAnnotation(XStreamAlias.class);
					if (null != xStreamAlias) {
						if (fieldAlias.equals(xStreamAlias.value())) {
							return true;
						}
					} else if (fieldAlias.equals(field.getName())) {
						return true;
					}
				}
			}

			return false;
		}
	}

	private static String cDATA(String text) {
		return PREFIX_CDATA + text + SUFFIX_CDATA;
	}
}
