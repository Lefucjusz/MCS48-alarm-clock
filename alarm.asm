;MAB8048H, 10.000MHz
;Gdansk 2021
	.cr	8048
	.tf	rom.bin,BIN
	.lf	alarm.lst
;======================== Defines ========================
;Pins
buzzer_pin      .eq %00100000 ;Buzzer pin at P1.5
rs_pin          .eq %01000000 ;LCD E pin at P1.6
e_pin           .eq %10000000 ;LCD RS pin at P1.7

;DS12887 registers
sec_reg	.eq $00
min_reg .eq $02
hr_reg	.eq $04
day_reg	.eq $07
mth_reg	.eq $08
yr_reg	.eq $09
a_reg	.eq $0A
b_reg	.eq $0B
c_reg	.eq $0C
d_reg	.eq $0D

;RAM variables
hours           .eq $20
minutes         .eq $21
seconds         .eq $22
days            .eq $23
months          .eq $24
years           .eq $25
alarm_hours     .eq $26
alarm_minutes   .eq $27
alarm_enabled   .eq $28
alarm_fired     .eq $29

;Constants
lcd_bell_glyph  .eq 0 ;Custom bell glyph address in LCD CGRAM
timer_init_val  .eq 6 ;Gives interrupt every 12ms @10MHz
cntr_init_val   .eq 17 ;Counter initial value, gives overflow every 204ms (17*12ms)

;Fixed purpose registers
;F0 - used to divide main counter by two, used for display blinking
;F1 - RTC interrupt flag
;R4 - pointer to currently changed value during time setting
;R5 - main counter used for timing things like buttons update, display blinking...

;======================== Macros ========================
sub     .ma A,V ;Subtracts V (Rx or immediate value) from A (A = A - V)
        cpl A
        add A,]2
        cpl A
        .em

;======================== Vectors ========================
	.no $00 ;Set jump to main at reset vector (00h)
	jmp main

    .no $03 ;Set jump to external interrupt ISR at external interrupt vector (03h)
	jmp rtc_isr

    .no $07 ;Set jump to timer interrupt ISR at timer interrupt vector (07h)
	jmp timer_isr

main:
    clr A ;Clear A
    mov R4,A ;Initialize value pointer 
    mov R0,#alarm_enabled
    mov @R0,A ;Disable alarm
    mov R0,#alarm_fired
    mov @R0,A ;Turn off alarm
    mov R0,#alarm_hours
    mov @R0,A ;Clear alarm hours
    mov R0,#alarm_minutes
    mov @R0,A ;Clear alarm minutes
    outl P1,A ;Clear P1 to turn off buzzer
    call lcd_init ;Initalize LCD
    call rtc_init ;Initialize RTC
    call timer_init ;Initalize hardware timer
    mov R5,#cntr_init_val ;Initialize counter
    en i ;Enable external interrupt
    en tcnti ;Enable timer interrupt
    strt t ;Start timer
    clr F1 ;Set RTC interrupt flag, so that update will trigger immediately after entering main loop
loop:
    jf1 skip_time_update ;If RTC interrupt flag cleared, no need to update time
    call rtc_get_time ;Otherwise obtain current time
    call alarm_update ;Check if alarm should fire
    cpl F1 ;Clear interrupt flag
skip_time_update:
    mov A,R5
    jnz loop ;If R5 != 0, loop
    cpl F0 ;Otherwise complement F0
    mov R5,#cntr_init_val ;Reload counter
    jf0 skip_buttons_update ;If F0 cleared, do not update buttons state
    call buttons_update ;Otherwise update buttons state
skip_buttons_update:
    call display_update ;Update display
    call buzzer_update ;Update buzzer state
    jmp loop ;Loop forever

;======================== LCD routines ========================
;R0 - byte, R1 - cmd/data switch, uses R0,R1, corrupts nothing
lcd_write:
	anl P1,#~rs_pin ;Clear RS
	;Test whether data or cmd will be sent
	mov A,R1 ;Load R1 to A to test if zero
	jz skip_rs ;Skip RS pin setting - cmd will be sent
	orl P1,#rs_pin ;Set RS pin - data will be sent
skip_rs:
	;Send upper nibble
	mov A,R0 ;Load byte to A
    outl P2,A ;Write A to P2
	orl P1,#e_pin ;Set E pin	
	anl P1,#~e_pin ;Clear E pin
	;Send lower nibble
    swap A ;Swap nibbles
    outl P2,A ;Write A to P2
	orl P1,#e_pin ;Set E pin	
	anl P1,#~e_pin ;Clear E pin	
	ret
	
;Uses and corrupts R0,R1,R6,R7	
lcd_cls:
	mov R1,#0 ;Send commands
	mov R0,#$01	
	call lcd_write ;Clear display
	mov R6,#1
	call delay_ms ;Wait 1ms
	mov R0,#$80
	call lcd_write ;Set cursor at first place in upper row
	mov R6,#1
	call delay_ms ;Wait 1ms
	ret

;R1 - row, R0 - column, uses and corrupts R0,R1	
lcd_gotoxy:
	mov A,R1
	jnz lcd_gotoxy_sec_row ;Check row
	mov A,#$80 ;If first, load address of its first position
	jmp lcd_gotoxy_write
lcd_gotoxy_sec_row:
	mov A,#$C0 ;If second, load address of its first position
lcd_gotoxy_write:
	add A,R0 ;Add offset (y)
	mov R0,A
	mov R1,#0
	call lcd_write ;Send command
	ret

;R2 - value to be displayed, uses R0,R1,R2
lcd_num:
	mov R0,#0 ;Clear tens
	mov R1,#1 ;Chars will be sent to display
div10:
	mov A,R2 ;Load value to be displayed to A
	cpl A ;Complement value
	add A,#10 ;Add 10 (C = (R2 < 10))
	jc div10_end ;If there has been carry - break
	cpl A ;Complement A (A=R2-10)
	mov R2,A ;Store new value in R2
	inc R0 ;Increment tens
	jmp div10 ;Perform again, until R2 < 10	
div10_end:
	mov A,R0
	add A,#$30 ;Add ASCII code for '0'
	mov R0,A
	call lcd_write ;Display tens
	mov A,R2 
	add A,#$30 ;Add ASCII code for '0'
	mov R0,A
	call lcd_write ;Display ones
	ret
 
    .ot
bell_glyph  .da #%00100, #%01110, #%01110, #%01110, #%11111, #%00000, #%00100, #%00000

lcd_load_glyph:
    mov R0,#$40 ;First custom CGRAM address
    mov R1,#0 ;Send command to display
    call lcd_write ;Set display to write to first custom CGRAM address
    mov R1,#1 ;Since now only characters will be sent to display
    mov R2,#bell_glyph ;Load bell_glyph array address to R2
    mov R7,#8 ;Load loop counter
lcd_load_glyphs_loop:
    mov A,R2
    movp A,@A ;Load glyph byte from ROM to A
    .ct
    mov R0,A ;Load glyph byte to R0
    call lcd_write ;Write character to LCD
    inc R2 ;Move pointer to next bell_glyph byte
    djnz R7,lcd_load_glyphs_loop ;Repeat for every byte from glyph
    call lcd_cls ;Clear display and move pointer to first row, first column
    ret
	
;Uses and corrupts R0,R1,R6,R7	
lcd_init:
	mov R1,#0 ;Whole subroutine will be sending commands
    mov R0,#$30	
	call lcd_write ;Weird 4-bit init command first time...
	mov R6,#5
	call delay_ms ;Wait 5ms
	mov R0,#$30
	call lcd_write ;Weird repeated 4-bit init command second time...
	mov R6,#1
	call delay_ms ;Wait 1ms
	mov R0,#$30
	call lcd_write ;Weird repeated 4-bit init command third time...
	mov R0,#$02
	call lcd_write ;Init 4-bit mode
	mov R0,#$28
	call lcd_write ;2 lines, 5*8 font, 4-bit mode
	mov R0,#$0C
	call lcd_write ;Display on, cursor off, blink off
	mov R0,#$06
	call lcd_write ;Autoincrement cursor position, text scroll off
    call lcd_load_glyph ;Load bell glyph
	ret

;Uses R0,R1	
rtc_init:
    mov R0,#a_reg
    mov A,#%00100000
    movx @R0,A ;Enable oscillator
    mov R0,#b_reg
    mov A,#%00010110
    movx @R0,A ;Enable update-ended interrupt, binary data format, 24h mode
	ret

rtc_disable_updates:
    mov R0,#b_reg
    movx A,@R0 ;Read DS12887 register B value
    orl A,#%10000000 ;Set SET bit
    anl A,#~%00010000 ;Disable update-ended interrupt
    movx @R0,A ;Save DS12887 new register B value
    ret

rtc_enable_updates:
    mov R0,#b_reg
    movx A,@R0 ;Read DS12887 register B value
    anl A,#~%10000000 ;Clear SET bit
    orl A,#00010000 ;Enable update-ended interrupt
    movx @R0,A ;Save DS12887 new register B value
    ret

rtc_write_hours:
    call rtc_disable_updates
    mov R0,#hr_reg
    mov R1,#hours
    mov A,@R1 ;Load hours value from RAM to A
    movx @R0,A ;Store hours value in hours register of RTC
    call rtc_enable_updates
    ret

rtc_write_minutes:
    call rtc_disable_updates
    mov R0,#min_reg
    mov R1,#minutes
    mov A,@R1 ;Load minutes value from RAM to A
    movx @R0,A ;Store minutes value in minutes register of RTC
    mov R0,#sec_reg
    mov R1,#seconds
    mov A,@R1 ;Load seconds value from RAM to A
    movx @R0,A ;Store seconds value in seconds register of RTC
    call rtc_enable_updates
    ret

rtc_write_date:
    call rtc_disable_updates
    mov R0,#day_reg
    mov R1,#days
    mov A,@R1 ;Load days value from RAM to A
    movx @R0,A ;Store days value in days register of RTC
    mov R0,#mth_reg
    mov R1,#months
    mov A,@R1 ;Load months value from RAM to A
    movx @R0,A ;Store months value in months register of RTC
    mov R0,#yr_reg
    mov R1,#years
    mov A,@R1 ;Load years value from RAM to A
    movx @R0,A ;Store years value in years register of RTC
    call rtc_enable_updates
    ret

rtc_get_time:
    mov R0,#hr_reg
    movx A,@R0 ;Read hours register
    mov R0,#hours
    mov @R0,A ;Store hours in RAM
    mov R0,#min_reg
    movx A,@R0 ;Read minutes register
    mov R0,#minutes
    mov @R0,A ;Store minutes in RAM
    mov R0,#sec_reg
    movx A,@R0 ;Read seconds register
    mov R0,#seconds
    mov @R0,A ;Store seconds in RAM
    mov R0,#day_reg
    movx A,@R0 ;Read days register
    mov R0,#days
    mov @R0,A ;Store days in RAM
    mov R0,#mth_reg
    movx A,@R0 ;Read months register
    mov R0,#months
    mov @R0,A ;Store months in RAM
    mov R0,#yr_reg
    movx A,@R0 ;Read years register
    mov R0,#years
    mov @R0,A ;Store years in RAM
    ret

rtc_isr:
    mov R0,#c_reg
    movx A,@R0 ;Clear DS12887 UF flag (it is cleared by reading Register C)
    clr F1 ;Set interrupt flag
    retr

timer_init:
    mov A,#timer_init_val
    mov T,A ;Load initial value to timer
    ret

timer_isr:
    dec R5 ;Decrement R5
    call timer_init ;Reinitialize timer
    retr

    .ot
month_days  .db 31,28,31,30,31,30,31,31,30,31,30,31

clock_setup:
    mov A,R4 ;switch(R4)
    jz set_alarm ;case 0
    dec A
    jz set_hours ;case 1
    dec A
    jz set_minutes ;case 2
    dec A
    jz set_days ;case 3
    dec A
    jz set_months ;case 4
    dec A
    jz set_years ;case 5
    dec A
    jz set_alarm_hours ;case 6
    dec A
    jz set_alarm_minutes ;case 7
    jmp clock_setup_end ;default

set_alarm:
    mov R0,#alarm_enabled
    mov A,@R0 ;Load alarm state to A
    xrl A,#1 ;Toggle alarm state
    mov @R0,A ;Write new state back to RAM 
    jmp clock_setup_end
set_hours:
    mov R0,#hours
    inc @R0 ;Increment hours
    mov A,@R0 ;Load hours value from RAM
    >sub A,#24 ;A = A - 24 -> C = (A < 24)
    jc write_hours ;If hours < 24, continue
    mov @R0,#0 ;Otherwise clear hours
write_hours:
    call rtc_write_hours ;Save hours in RTC
    jmp clock_setup_end
set_minutes:
    mov R0,#seconds
    mov @R0,#0 ;Clear seconds
    mov R0,#minutes
    inc @R0 ;Increment minutes
    mov A,@R0 ;Load minutes value from RAM
    >sub A,#60 ;A = A - 60 -> C = (A < 60)
    jc write_minutes ;If minutes < 60, continue
    mov @R0,#0 ;Otherwise clear minutes
write_minutes:
    call rtc_write_minutes
    jmp clock_setup_end
set_days:
    mov R0,#months 
    mov A,@R0 ;Load current months value
    dec A ;Decrement months value to obtain proper array offset value
    mov R0,#month_days ;Load month_days array address
    add A,R0 ;Add offset
    movp A,@A ;Load number of days from array
    .ct ;TODO
    inc A ;Add one day for easier comparison later
    mov R0,A ;Store number of days in R0
    mov R1,#days
    inc @R1 ;Increment days
    mov A,@R1 ;Load days value from RAM
    >sub A,R0 ;A = A - month_days[month] -> C = (A < month_days[month])
    jc write_days ;If days < month_days[month], continue
    mov @R1,#1 ;Otherwise clear days
write_days:
    call rtc_write_date ;Save days in RTC
    jmp clock_setup_end
set_months:
    mov R0,#months
    inc @R0 ;Increment months
    mov A,@R0 ;Load months value from RAM
    >sub A,#13 ;A = A - 13 -> C = (A < 13)
    jc write_months ;If months < 13, continue
    mov @R0,#1 ;Otherwise clear months
write_months:
    call rtc_write_date ;Save months in RTC
    jmp clock_setup_end
set_years:
    mov R0,#years
    inc @R0 ;Increment years
    mov A,@R0 ;Load years value from RAM
    >sub A,#100 ;A = A - 100 -> C = (A < 100)
    jc write_years ;If years < 100, continue
    mov @R0,#0 ;Otherwise clear years
write_years:
    call rtc_write_date ;Save years in RTC
    jmp clock_setup_end
set_alarm_hours:
    mov R0,#alarm_hours
    inc @R0 ;Increment alarm_hours
    mov A,@R0 ;Load alarm_hours value from RAM
    >sub A,#24 ;A = A - 24 -> C = (A < 24)
    jc clock_setup_end ;If alarm_hours < 24, continue
    mov @R0,#0 ;Otherwise clear alarm_hours
    jmp clock_setup_end
set_alarm_minutes:
    mov R0,#alarm_minutes
    inc @R0 ;Increment alarm_minutes
    mov A,@R0 ;Load alarm_minutes value from RAM
    >sub A,#60 ;A = A - 60 -> C = (A < 60)
    jc clock_setup_end ;If alarm_minutes < 60, continue
    mov @R0,#0 ;Otherwise clear alarm_minutes
clock_setup_end:
    ret

display_update:
    mov R0,#0
    mov R1,#0
    call lcd_gotoxy
    mov R0,#hours
    mov A,@R0
    mov R2,A ;Load hours from RAM to R2
    mov R3,#1 ;Hours should blink when position pointer is at first position
    call display_num_with_blink
    mov R0,#':'
    call lcd_write ;Display ':'
    mov R0,#minutes
    mov A,@R0
    mov R2,A ;Load minutes from RAM to R2
    mov R3,#2 ;Minutes should blink when position pointer is at second position
    call display_num_with_blink
    mov R0,#':'
    call lcd_write ;Display ':'
    mov R0,#seconds
    mov A,@R0
    mov R2,A ;Load seconds from RAM to R2
    call lcd_num ;Seconds are not being set, so no blinking needed
    mov R1,#0
    mov R0,#10
    call lcd_gotoxy ;Go to first row, tenth column
    mov R0,#days
    mov A,@R0
    mov R2,A ;Load days from RAM to R2
    mov R3,#3 ;Days should blink when position pointer is at third position
    call display_num_with_blink
    mov R0,#'-'
    call lcd_write ;Display '-'
    mov R0,#months
    mov A,@R0
    mov R2,A ;Load months from RAM to R2
    mov R3,#4 ;Months should blink when position pointer is at fourth position
    call display_num_with_blink
    mov R0,#'-'
    call lcd_write
    mov R0,#'2'
    call lcd_write
    mov R0,#'0'
    call lcd_write ;Display '-20'
    mov R0,#years
    mov A,@R0
    mov R2,A ;Load years from RAM to R2
    mov R3,#5 ;Years should blink when position pointer is at fifth position
    call display_num_with_blink

    mov R0,#6
    mov R1,#1
    call lcd_gotoxy ;Go to second row, sixth column
    mov R1,#1 ;Send characters to display

    mov A,R4 ;Load position pointer to A
    jnz display_alarm ;Display alarm values only if in setup mode, otherwise erase it
    mov R0,#alarm_enabled
    mov A,@R0 ;Load alarm state
    jz erase_alarm ;If alarm not enabled
display_alarm:
    mov R0,#lcd_bell_glyph
    call lcd_write ;Display bell
    mov R0,#' '
    call lcd_write ;Display ' '
    mov R0,#alarm_hours
    mov A,@R0
    mov R2,A ;Load alarm hours from RAM to R2
    mov R3,#6 ;Alarm hours should blink when position pointer is at sixth position
    call display_num_with_blink
    mov R0,#':'
    call lcd_write ;Display ':'
    mov R0,#alarm_minutes
    mov A,@R0
    mov R2,A ;Load alarm minutes from RAM to R2
    mov R3,#7 ;Alarm minutes should blink when position pointer is at seventh position
    call display_num_with_blink
    mov R0,#' '
    call lcd_write ;Display ' '
    mov R0,#lcd_bell_glyph
    call lcd_write ;Display bell
    jmp display_update_end
erase_alarm:
    mov R0,#' '
    mov R7,#9
erase_alarm_loop:
    call lcd_write
    djnz R7,erase_alarm_loop
display_update_end:
    ret

;R2 - value to be displayed, R3 - value of position counter for which the displayed value should blink
display_num_with_blink:
    mov A,R4 ;Load position pointer to A
    >sub A,R3 ; A = A - R3
    jnz skip_blinking ;If currently selected value is different than the one for which the value should blink, do not blink
    jf0 blank_value ;Otherwise perform blinking, if F0 set - blank value, otherwise display value
skip_blinking:
    call lcd_num ;Display value
    jmp display_num_end ;Finish
blank_value:
    mov R0,#' '
    mov R1,#1 ;Send chars to LCD
    call lcd_write
    call lcd_write ;Display ' ' twice
display_num_end:
    ret

buttons_update:
    jt0 check_value_button
    mov R0,#alarm_fired
    mov A,@R0 ;Load alarm_fired flag to A
    jz buttons_update_no_alarm ;Check if alarm fired, if not, proceed to normal functions
    dec A
    mov @R0,A ;Turn off alarm
    jmp buttons_update_end ;Skip normal functionality
buttons_update_no_alarm
    mov A,R4
    inc A
    anl A,#7 ;A = A & 7 = A % 8
    mov R4,A
check_value_button:
    jt1 buttons_update_end
    call clock_setup
buttons_update_end:
    ret

alarm_update:
    mov R0,#alarm_enabled
    mov A,@R0
    jz alarm_update_end ;If alarm not enabled, finish    
    mov R0,#hours
    mov A,@R0
    mov R0,A ;Load current hours value to R0
    mov R1,#alarm_hours
    mov A,@R1 ;Load alarm hours to A
    >sub A,R0 ; A = A - R0 -> Z = 1 if (A == R0) -> Z = 1 if (hours == alarm_hours)
    jnz alarm_update_end ;If hours != alarm_hours, finish
    mov R0,#minutes
    mov A,@R0
    mov R0,A ;Load current minutes value to R0
    mov R1,#alarm_minutes
    mov A,@R1 ;Load alarm minutes to A
    >sub A,R0 ; A = A - R0 -> Z = 1 if (A == R0) -> Z = 1 if (minutes == alarm_minutes)
    jnz alarm_update_end ;If minutes != alarm_minutes, finish
    mov R0,#seconds
    mov A,@R0 ;Load current seconds value
    jnz alarm_update_end ;If seconds != 0, finish
    mov R0,#alarm_fired
    inc @R0 ;Fastest way to set alarm_fired flag
alarm_update_end:
    ret

buzzer_update:
    mov R0,#alarm_fired
    mov A,@R0
    jnz buzzer_toggle ;If alarm fired, make interrupted beeping sound
    in A,P1 ;Load current P1 state to A
    anl A,#~buzzer_pin ;Turn off buzzer
    outl P1,A ;Write back to P1
    jmp buzzer_update_end
buzzer_toggle:
    in A,P1 ;Load current P1 state to A
    xrl A,#buzzer_pin ;Toggle buzzer
    outl P1,A ;Write back to P1
buzzer_update_end:
    ret

delay_ms:
    mov R7,#228
delay_ms_loop:
    nop
    djnz R7,delay_ms_loop
    djnz R6,delay_ms
    ret
