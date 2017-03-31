package cn.blmdz.hunt.protocol;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target({
	ElementType.METHOD,
	ElementType.TYPE
	})
public @interface Export {
	public abstract String[] value();

	public abstract String[] paramNames();
}